-module(rcl_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 2, period => 10},
    ChildSpecs =
        [{rcl_bench_stats,
          {rcl_bench_stats, start_link, []},
          permanent,
          5000,
          worker,
          [rcl_bench_stats]},
         {rcl_bench_workers_sup,
          {rcl_bench_workers_sup, start_link, []},
          permanent,
          5000,
          supervisor,
          [rcl_bench_workers_sup]}],
    {ok, {SupFlags, ChildSpecs}}.
