-module(rcl_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    create_tables(),

    SupFlags = #{strategy => one_for_all, intensity => 2, period => 10},
    ChildSpecs =
        [
         {rcl_bench_sample_slide_sup,
          {rcl_bench_sample_slide_sup, start_link, []},
          permanent,
          5000,
          worker,
          [rcl_bench_sample_slide_sup]},
         {rcl_bench_stats,
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


create_tables() ->
    Tables = [
              {rcl_bench, [set, named_table, public, {read_concurrency, true}]},
              {rcl_bench_histogram, [set, named_table, public, {write_concurrency, true}]}
             ],
    [ets:new(Name, Opts) || {Name, Opts} <- Tables],
    ok.