%%%-------------------------------------------------------------------
%% @doc rcl_bench public API
%% @end
%%%-------------------------------------------------------------------

-module(rcl_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:debug("Preparing results directory"),
    prepare_test_dir(),

    log_dimensions(),

    {ok, DriverMod} = application:get_env(rcl_bench, driver_module),
    % spawn duration exit
    {ok, DurationMins} = DriverMod:duration(),
    Duration = timer:minutes(DurationMins) + timer:seconds(1),
    spawn_link(fun () ->
                       receive
                           after Duration ->
                                     logger:notice("Benchmark finished"),
                                     init:stop(0)
                       end
               end),

    logger:debug("Starting application"),
    rcl_bench_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

id() ->
    {A, B, C} = erlang:timestamp(),
    lists:flatten(io_lib:format("~p.~p.~p", [A, B, C])).

prepare_test_dir() ->
    {ok, CWD} = file:get_cwd(),
    ResultsDir = filename:join([CWD, "tests"]),
    TestDir = filename:join([ResultsDir, id()]),
    ok = filelib:ensure_dir(filename:join(TestDir, "foobar")),
    Link = filename:join([ResultsDir, "current"]),
    [] =
        os:cmd(io_lib:format("rm -f ~s; mkdir -p ~s; ln -sf ~s ~s",
                             [Link, ResultsDir, TestDir, Link])),
    application:set_env(rcl_bench, current_dir, TestDir),
    TestDir.

log_dimensions() ->
    {ok, DriverMod} = application:get_env(rcl_bench, driver_module),
    {ok, KeyGenerator} = DriverMod:key_generator(),
    case rcl_bench_keygen:dimension(KeyGenerator) of
        undefined ->
            logger:notice("No dimensions for ~p", [KeyGenerator]),
            ok;
        Keyspace ->
            {ok, ValueGenerator} = DriverMod:value_generator(),
            ValueSpace = rcl_bench_valgen:dimension(ValueGenerator, Keyspace),
            {Size, Desc} = rcl_bench_util:user_friendly_bytes(ValueSpace),
            logger:notice("(~p, ~p) Estimated data size: ~.2f ~p",
                          [KeyGenerator, ValueGenerator, Size, Desc])
    end.
