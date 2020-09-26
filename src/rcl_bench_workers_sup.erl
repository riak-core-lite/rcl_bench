-module(rcl_bench_workers_sup).

-behaviour(supervisor).

-export([start_link/0, workers/0, stop_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

workers() ->
    [Pid || {_Id, Pid, worker, [rcl_bench_worker]} <- supervisor:which_children(?MODULE)].

stop_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).

init([]) ->
    {ok, DriverMod} = application:get_env(rcl_bench, driver_module),
    {ok, ConcurrentWorkers} = DriverMod:concurrent_workers(),
    logger:notice("Concurrent workers: ~p", [ConcurrentWorkers]),
    Workers = worker_specs(ConcurrentWorkers, DriverMod, []),

    SupFlags = #{strategy => one_for_all, intensity => 5, period => 10},
    {ok, {SupFlags, Workers}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

worker_specs(0, _DriverMod, Acc) ->
    Acc;
worker_specs(Count, DriverMod, Acc) ->
    Id = list_to_atom(lists:concat([rcl_bench_worker_, Count])),
    Spec =
        {Id,
         {rcl_bench_worker, start_link, [Id, Count, DriverMod]},
         permanent,
         5000,
         worker,
         [rcl_bench_worker]},
    worker_specs(Count - 1, DriverMod, [Spec | Acc]).
