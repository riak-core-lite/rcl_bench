-module(rcl_bench_stats).

-behaviour(gen_server).

%% API
-export([start_link/0, exponential/1, run/0, op_complete/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {ops, report_interval, start_time = os:timestamp(), last_write_time = os:timestamp()}).


report_interval() -> 5.

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

    ets:new(t, [set, named_table]),


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

    %% Setup a histogram for each operation 
    %% we only track latencies on successful operations
    [begin
        rcl_bench_histogram:new_histogram({latencies, Op}, report_interval())
    end || Op <- Ops],

    ReportInterval = timer:seconds(report_interval()),
    timer:send_interval(ReportInterval, report),

    [erlang:put({csv_file, X}, op_csv_file(X, DriverMod)) || X <- Ops],
    erlang:put({csv_file, summary_file}, op_summary_csv_file(DriverMod)),
    {ok, #state{ops = Ops, report_interval = ReportInterval}}.

handle_call({op, Op, {error, _Reason}, _ElapsedUs}, _From, State) ->
    ets:update_counter(rcl_bench_errors, Op, [{2,1}], {Op, 0}),
    {reply, ok, State};
handle_call({write, {Op, Units, ElapsedUs}}, _From, State) ->
    ets:update_counter(t, Op, [{2,Units}], {Op, 0}),
    rcl_bench_histogram:notify({latencies, Op}, ElapsedUs),
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(report, State) ->
    Now = os:timestamp(),

    Elapsed = timer:now_diff(Now, State#state.start_time) / 1000000,
    Window  = timer:now_diff(Now, State#state.last_write_time) / 1000000,

    {Oks, Errors, _OkOpsRes} =
        lists:foldl(fun(Op, {TotalOks, TotalErrors, OpsResAcc}) ->
            Oks = case ets:lookup(t, Op) of
                [{Op, EtsOks}] -> EtsOks;
                [] -> 0
            end,
            OpErrors = case ets:lookup(rcl_bench_errors, Op) of
                [{Op, EtsErrs}] -> EtsErrs;
                [] -> 0
            end,

            Stats = rcl_bench_histogram:get_histogram_statistics({latencies, Op}),
            P = proplists:get_value(percentile, Stats),

            ets:update_counter(t, Op, {2, -Oks}, {Op, 0}),
            ets:update_counter(rcl_bench_errors, Op, {2, -OpErrors}, {Op, 0}),
            ets:update_counter(rcl_bench_total_errors, Op, {2, OpErrors}, {Op, 0}),

            %% write single files
            %% elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors
            %% Write summary
            File = erlang:get({csv_file, Op}),
            file:write(File,
                io_lib:format("~w, ~w, ~w, ~w, ~.1f, ~w, ~w, ~w, ~w, ~w, ~w\n",
                    [Elapsed,
                        Window,
                        Oks,
                        proplists:get_value(min, Stats, 0),
                        proplists:get_value(arithmetic_mean, Stats, 0),
                        proplists:get_value(median, Stats, 0),
                        proplists:get_value(95, P, 0),
                        proplists:get_value(99, P, 0),
                        proplists:get_value(999, P, 0),
                        proplists:get_value(max, Stats, 0),
                        OpErrors])),
            
            %% TODO report total errors
            {TotalOks + Oks, TotalErrors + OpErrors,
                [{Op, Oks}|OpsResAcc]}
                    end, {0,0,[]}, State#state.ops),

    %% Write summary
    File = erlang:get({csv_file, summary_file}),
    file:write(File,
        io_lib:format("~w, ~w, ~w, ~w, ~w\n",
            [Elapsed,
                Window,
                Oks + Errors,
                Oks,
                Errors])),

    {noreply, State#state { last_write_time = Now }}.

terminate(_Reason, State) ->
    % report one last time
    handle_info(report, State),
    report_total_errors(State),

    % close files
    [ok = file:close(F) || {{csv_file, _}, F} <- erlang:get()],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

op_summary_csv_file(DriverMod) ->
    {ok, TestDir2} = DriverMod:test_dir(),
    TestDir = filename:join(TestDir2, "current"),
    Fname = filename:join(TestDir, "summary.csv"),
    {ok, F} = file:open(Fname, [raw, binary, write]),
    ok = file:write(F, <<"elapsed, window, total, successful, failed\n">>),
    F.

op_csv_file({Label, _Op}, DriverMod) ->
    {ok, TestDir2} = DriverMod:test_dir(),
    TestDir = filename:join(TestDir2, "current"),
    Fname = filename:join(TestDir, rcl_bench_util:normalize_label(Label) ++ "_latencies.csv"),
    {ok, F} = file:open(Fname, [raw, binary, write]),
    file:write(F, <<"elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n">>),
    F.

report_total_errors(_State) ->
    FilteredErrors = lists:filter(fun({_Op, Count}) -> Count > 0 end, ets:tab2list(rcl_bench_total_errors)),
    logger:warning("Errors: ~p", [FilteredErrors]).