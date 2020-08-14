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
    {ok, ConcurrentWorkers} = application:get_env(rcl_bench, concurrent),
    logger:notice("Concurrent workers: ~p", [ConcurrentWorkers]),
    Workers = worker_specs(ConcurrentWorkers, []),

    SupFlags = #{strategy => one_for_all, intensity => 5, period => 10},
    {ok, {SupFlags, Workers}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

worker_specs(0, Acc) ->
    Acc;
worker_specs(Count, Acc) ->
    Id = list_to_atom(lists:concat([rcl_bench_worker_, Count])),
    Spec =
        {Id,
         {rcl_bench_worker, start_link, [Id, Count]},
         permanent,
         5000,
         worker,
         [rcl_bench_worker]},
    worker_specs(Count - 1, [Spec | Acc]).
