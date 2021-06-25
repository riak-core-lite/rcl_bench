-module(rcl_bench_worker).

-behaviour(gen_server).

%% API
-export([start_link/3, run/1, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state,
{id,
    keygen,
    valgen,
    driver_state,
    shutdown_on_error,
    ops,
    ops_len,
    parent_pid,
    worker_pid,
    sup_id,
    driver_mod}).

%% ====================================================================
%% API
%% ====================================================================

start_link(SupChild, Id, DriverMod) ->
    gen_server:start_link(?MODULE, [SupChild, Id, DriverMod], []).

run(Pids) ->
    [ok = gen_server:call(Pid, run) || Pid <- Pids],
    ok.

stop(Pids) ->
    [ok = gen_server:call(Pid, stop) || Pid <- Pids],
    ok.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([SupChild, Id, DriverMod]) ->
    %% Setup RNG seed for worker sub-process to use; incorporate the ID of
    %% the worker to ensure consistency in load-gen
    %%
    %% NOTE: If the worker process dies, this obviously introduces some entropy
    %% into the equation since you'd be restarting the RNG all over.
    %%
    %% The RNG_SEED is static by default for replicability of key size
    %% and value size generation between test runs.
    process_flag(trap_exit, true),

    Ops = ops_tuple(DriverMod),
    ShutdownOnError = DriverMod:shutdown_on_error(),

    %% Finally, initialize key and value generation. We pass in our ID to the
    %% initialization to enable (optional) key/value space partitioning
    {ok, KeyGenerator} = DriverMod:key_generator(),
    {ok, ValueGenerator} = DriverMod:value_generator(),
    KeyGenFun = rcl_bench_keygen:new(KeyGenerator, Id),
    ValGenFun = rcl_bench_valgen:new(ValueGenerator, Id),

    State =
        #state{id = Id,
            keygen = KeyGenFun,
            valgen = ValGenFun,
            shutdown_on_error = ShutdownOnError,
            ops = Ops,
            ops_len = size(Ops),
            parent_pid = self(),
            sup_id = SupChild,
            driver_mod = DriverMod},

    %% Use a dedicated sub-process to do the actual work. The work loop may need
    %% to sleep or otherwise delay in a way that would be inappropriate and/or
    %% inefficient for a gen_server. Furthermore, we want the loop to be as
    %% tight as possible for peak load generation and avoid unnecessary polling
    %% of the message queue.
    %%
    %% Link the worker and the sub-process to ensure that if either exits, the
    %% other goes with it.
    WorkerPid =
        spawn_link(fun () ->
            worker_init(State)
                   end),
    WorkerPid ! {init_driver, self()},
    receive
        driver_ready ->
            ok
    end,

    %% start
    gen_server:cast(self(), run),

    {ok, State#state{worker_pid = WorkerPid}}.

handle_call(run, _From, State) ->
    State#state.worker_pid ! run,
    {reply, ok, State}.

handle_cast(run, State) ->
    State#state.worker_pid ! run,
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
        normal ->
            %% Clean shutdown of the worker; spawn a process to terminate this
            %% process via the supervisor API and make sure it doesn't restart.
            spawn(fun () ->
                stop_worker(State#state.sup_id)
                  end),
            {noreply, State};
        _ ->
            logger:error("Worker ~p exited with ~p", [Pid, Reason]),
            %% Worker process exited for some other reason; stop this process
            %% as well so that everything gets restarted by the sup
            {stop, normal, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%
%% Stop a worker process via the supervisor and terminate the app
%% if there are no workers remaining
%%
%% WARNING: Must run from a process other than the worker!
%%
stop_worker(SupChild) ->
    ok = rcl_bench_workers_sup:stop_child(SupChild),
    case rcl_bench_workers_sup:workers() of
        [] ->
            %% No more workers -- stop the system
            rcl_bench_util:exit("Stopping the system", []);
        _ ->
            ok
    end.

%%
%% Expand operations list into tuple suitable for weighted, random draw
%%
ops_tuple(DriverMod) ->
    {ok, Operations} = DriverMod:operations(),
    F =
        fun ({OpTag, Count}) ->
            lists:duplicate(Count, {OpTag, OpTag});
            ({Label, OpTag, Count}) ->
                lists:duplicate(Count, {Label, OpTag})
        end,
    Ops = [F(X) || X <- Operations],
    list_to_tuple(lists:flatten(Ops)).

worker_init(State) ->
    %% Trap exits from linked parent process; use this to ensure the driver
    %% gets a chance to cleanup
    process_flag(trap_exit, true),

    Id = State#state.id,
    DriverMod = State#state.driver_mod,
    {ok, Timestamp} = {ok, {A1, A2, A3}} = DriverMod:random_seed(),
    {ok, Algorithm} = DriverMod:random_algorithm(),
    RngSeed = {A1 + Id, A2 + Id, A3 + Id},
    rand:seed(Algorithm, RngSeed),
    logger:notice("Using seed ~p with ~p for worker ~p", [Timestamp, Algorithm, Id]),

    worker_idle_loop(State).

worker_idle_loop(State) ->
    DriverMod = State#state.driver_mod,
    receive
        {init_driver, Caller} ->
            %% Spin up the driver implementation
            case catch DriverMod:new(State#state.id) of
                {ok, DriverState} ->
                    Caller ! driver_ready,
                    ok;
                Error ->
                    DriverState = undefined, % Make erlc happy
                    rcl_bench_util:exit("Failed to initialize driver: ~p", [Error])
            end,
            worker_idle_loop(State#state{driver_state = DriverState});
        run ->
            DriverMod = State#state.driver_mod,
            {ok, Mode} = DriverMod:mode(),
            case Mode of
                {rate, max} ->
                    logger:notice("Starting max worker: ~p", [self()]),
                    max_worker_run_loop(State);
                {rate, Rate} ->
                    %% Calculate mean inter-arrival time in in milliseconds. A
                    %% fixed rate worker can generate (at max) only 1k req/sec.
                    MeanArrival = 1000 / Rate,
                    logger:notice("Starting ~w ms/req fixed rate worker: ~p",
                        [MeanArrival, self()]),
                    rate_worker_run_loop(State, 1 / MeanArrival)
            end
    end.

worker_next_op2(State, OpTag) ->
    DriverMod = State#state.driver_mod,
    catch DriverMod:run(OpTag,
        State#state.keygen,
        State#state.valgen,
        State#state.driver_state).

worker_next_op(State) ->
    DriverMod = State#state.driver_mod,
    Next = element(rand:uniform(State#state.ops_len), State#state.ops),
    {_Label, OpTag} = Next,
    Start = os:timestamp(),
    Result = worker_next_op2(State, OpTag),
    ElapsedUs = erlang:max(0, timer:now_diff(os:timestamp(), Start)),
    case Result of
        {Res, DriverState} when Res == ok orelse element(1, Res) == ok ->
            rcl_bench_stats:op_complete(Next, Res, ElapsedUs),
            {ok, State#state{driver_state = DriverState}};
        {Res, DriverState} when Res == silent orelse element(1, Res) == silent ->
            {ok, State#state{driver_state = DriverState}};
        {error, Reason, DriverState} ->
            logger:warning("Error: ~p", [Reason]),
            %% Driver encountered a recoverable error
            rcl_bench_stats:op_complete(Next, {error, Reason}, ElapsedUs),
            State#state.shutdown_on_error andalso
                rcl_bench_util:exit("(~p) shutdown on error", [1]),
            {ok, State#state{driver_state = DriverState}};
        {'EXIT', Reason} ->
            logger:warning("Driver crashed: ~p", [Reason]),

            %% Driver crashed, generate a crash error and terminate. This will take down
            %% the corresponding worker which will get restarted by the appropriate supervisor.
            rcl_bench_stats:op_complete(Next, {error, {crash, Reason}}, ElapsedUs),

            %% Give the driver a chance to cleanup
            catch DriverMod:terminate({'EXIT', Reason}, State#state.driver_state),

            case State#state.shutdown_on_error of
                true ->
                    rcl_bench_util:exit("(~p) shutdown on crash", [2]);
                false ->
                    crash
            end;
        {stop, Reason} ->
            logger:notice("Driver (~p) has requested stop: ~p", [self(), Reason]),
            %% Give the driver a chance to cleanup
            catch DriverMod:terminate(normal, State#state.driver_state),
            normal
    end.

needs_shutdown(State) ->
    DriverMod = State#state.driver_mod,
    Parent = State#state.parent_pid,
    receive
        {'EXIT', Pid, _Reason} ->
            case Pid of
                Parent ->
                    %% Give the driver a chance to cleanup
                    catch DriverMod:terminate(normal, State#state.driver_state),
                    true;
                _Else ->
                    %% catch this so that selective receive doesn't kill us
                    false
            end
    after 0 ->
        false
    end.

max_worker_run_loop(State) ->
    case worker_next_op(State) of
        {ok, State2} ->
            case needs_shutdown(State2) of
                true ->
                    ok;
                false ->
                    max_worker_run_loop(State2)
            end;
        ExitReason ->
            exit(ExitReason)
    end.

rate_worker_run_loop(State, Lambda) ->
    %% Delay between runs using exponentially distributed delays to mimic
    %% queue.
    timer:sleep(trunc(rcl_bench_stats:exponential(Lambda))),
    case worker_next_op(State) of
        {ok, State2} ->
            case needs_shutdown(State2) of
                true ->
                    ok;
                false ->
                    rate_worker_run_loop(State2, Lambda)
            end;
        ExitReason ->
            exit(ExitReason)
    end.
