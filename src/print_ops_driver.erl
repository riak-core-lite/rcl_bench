-module(print_ops_driver).
-behaviour(rcl_bench_driver).

-export([new/1, run/4, terminate/2]).

-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
         key_generator/0, value_generator/0, random_algorithm/0,
         random_seed/0, shutdown_on_error/0]).

%% =======================
%% Benchmark configuration

mode() -> {ok, {rate, max}}.
%% Number of concurrent workers
concurrent_workers() -> {ok, 1}.
%% Test duration (minutes)
duration() -> {ok, 1}.
%% Operations (and associated mix)
operations() ->
    {ok, [{put, 10}, 
          {get, 70},
          {err, 0},
          {cr, 0}
          ]}.

%% Base test output directory
test_dir() -> {ok, "tests"}.

%% Key generators
%% {uniform_int, N} - Choose a uniformly distributed integer between 0 and N
key_generator() -> {ok, {pareto_int, 10}}.

%% Value generators
%% {fixed_bin, N} - Fixed size binary blob of N bytes
value_generator() -> {ok, {fixed_bin, 100}}.

random_algorithm() -> {ok, exsss}.
random_seed() -> {ok, {1,4,3}}.

shutdown_on_error() -> false.



%% ========================
%% Benchmark implementation

new(Id) ->
    {ok, #{}}.

run(err, _KeyGen, _ValueGen, State) ->
    % you can return any error term you want
    {error, {error, wanted}, State};
run(cr, _KeyGen, _ValueGen, State) ->
    % this operation will crash and counts as an error for operation cr
    % if the worker crashes too often, the benchmark will stop
    1/0,
    {ok, State};
run(get, KeyGen, _ValueGen, State) ->
    % a fast operation
    Key =  KeyGen(),
    %logger:notice("Get: ~p", [Key]),
    Fun = fun(V) -> V + 1 end,
    {ok, maps:update_with(Key,Fun,0,State)};
run(put, _KeyGen, _ValueGen, State) ->
    % a slow operation
    timer:sleep(round((rand:uniform())*100)),
    {ok, State}.

terminate(_, State) ->
  logger:notice("Finished: ~p", [State]),
  ok.









