-module(rcl_bench_stats).

-behaviour(gen_server).

%% API
-export([start_link/0, exponential/1, run/0, op_complete/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-record(state, {ops}).

%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

exponential(Lambda) ->
    -math:log(rand:uniform()) / Lambda.

run() ->
    gen_server:call(?MODULE, run).

op_complete(Op, ok, ElapsedUs) ->
    op_complete(Op, {ok, 1}, ElapsedUs);
op_complete(Op, {ok, Units}, ElapsedUs) ->
    gen_server:call(?MODULE, {write, {Op, Units, ElapsedUs}});
op_complete(Op, Result, ElapsedUs) ->
    gen_server:call(?MODULE, {op, Op, Result, ElapsedUs}).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([]) ->
    %%    %% Trap exits so we have a chance to flush data
    process_flag(trap_exit, true),
    process_flag(priority, high),

    %% Initialize an ETS table to track error and crash counters during
    %% reporting interval
    ets:new(rcl_bench_errors, [protected, named_table]),

    %% Initialize an ETS table to track error and crash counters since
    %% the start of the run
    ets:new(rcl_bench_total_errors, [protected, named_table]),

    {ok, DriverMod} = application:get_env(rcl_bench, driver_module),
    {ok, Operations} = DriverMod:operations(),
    %% Get the list of operations we'll be using for this test
    F1 =
        fun ({OpTag, _Count}) ->
                {OpTag, OpTag};
            ({Label, OpTag, _Count}) ->
                {Label, OpTag}
        end,
    Ops = [F1(X) || X <- Operations],

    logger:notice("Operations: ~p", [Ops]),
    [erlang:put({csv_file, X}, op_csv_file(X, DriverMod)) || X <- Ops],
    {ok, #state{ops = Ops}}.

handle_call({op, Op, {error, Reason}, _ElapsedUs}, _From, State) ->
    increment_error_counter(Op),
    increment_error_counter({Op, Reason}),
    {reply, ok, State};
handle_call({write, {Op, Units, ElapsedUs}}, _From, State) ->
    Line = io_lib:format("~w, ~w, ~w\n", [os:system_time(millisecond), Units, ElapsedUs]),
    File = erlang:get({csv_file, Op}),
    ok = file:write(File, Line),
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    report_total_errors(State),

    [ok = file:close(F) || {{csv_file, _}, F} <- erlang:get()],
    %%    ok = file:close(State#state.summary_file),
    %%    ok = file:close(State#state.errors_file),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

op_csv_file({Label, _Op}, DriverMod) ->
    {ok, TestDir2} = DriverMod:test_dir(),
    TestDir = filename:join(TestDir2, "current"),
    Fname = filename:join(TestDir, rcl_bench_util:normalize_label(Label) ++ "_single.csv"),
    {ok, F} = file:open(Fname, [raw, binary, write]),
    ok = file:write(F, <<"timestamp, unit, microseconds\n">>),
    F.

increment_error_counter(Key) ->
    ets_increment(rcl_bench_errors, Key, 1).

ets_increment(Tab, Key, Incr) when is_integer(Incr) ->
    %% Increment the counter for this specific key. We have to deal with
    %% missing keys, so catch the update if it fails and init as necessary
    case catch ets:update_counter(Tab, Key, Incr) of
        Value when is_integer(Value) ->
            ok;
        {'EXIT', _} ->
            case ets:insert_new(Tab, {Key, Incr}) of
                true ->
                    ok;
                _ ->
                    %% Race with another load gen proc, so retry
                    ets_increment(Tab, Key, Incr)
            end
    end.

report_total_errors(State) ->
    case ets:tab2list(rcl_bench_errors) of
        [] ->
            logger:notice("No Errors");
        UnsortedErrCounts ->
            ErrCounts = lists:sort(UnsortedErrCounts),
            F =
                fun ({Key, _Count}) ->
                        lists:member(Key, State#state.ops)
                end,
            ErrorSummary = lists:filter(F, ErrCounts),
            logger:notice("Total Errors: ~p", [ErrorSummary])
    end.
