-module(rcl_bench_driver).
-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
         key_generator/0, value_generator/0, random_algorithm/0,
         random_seed/0, shutdown_on_error/0]).

-type state() :: term().
-type command() :: term().
-type keygen() :: term().
-type valuegen() :: term().
-type error() :: {error, term()}.

-type mode_config() :: term().
-type key_generator_config() :: term().
-type value_generator_config() :: term().
-type random_algorithm_config() :: term().
-type random_seed_value() :: term().

-callback new(term()) -> {ok, state()}.
-callback run(command(), keygen(), valuegen(), state()) -> {ok, state()} | error().
-callback terminate(term(), state()) -> ok.

-callback mode() -> {ok, mode_config()}.
-callback concurrent_workers() -> {ok, pos_integer()}.
-callback duration() -> {ok, pos_integer()}.
-callback operations() -> {ok, [{command(), pos_integer()}]}.
-callback test_dir() -> {ok, nonempty_string()}.
-callback key_generator() -> {ok, key_generator_config()}.
-callback value_generator() -> {ok, value_generator_config()}.
-callback random_algorithm() -> {ok, random_algorithm_config()}.
-callback random_seed() -> {ok, random_seed_value()}.
-callback shutdown_on_error() -> boolean().

% default implementations to reuse in drivers

mode() -> {ok, {rate, max}}.
%% Number of concurrent workers
concurrent_workers() -> {ok, 2}.
%% Test duration (minutes)
duration() -> {ok, 1}.
%% Operations (and associated mix)
operations() ->
    {ok, [{get_own_puts, 3}, 
          {put, 10}, 
          {get, 2}]}.

%% Base test output directory
test_dir() -> {ok, "tests"}.


%% Key generators
%% {uniform_int, N} - Choose a uniformly distributed integer between 0 and N
key_generator() -> {ok, {uniform_int, 100000}}.

%% Value generators
%% {fixed_bin, N} - Fixed size binary blob of N bytes
value_generator() -> {ok, {fixed_bin, 100}}.

random_algorithm() -> {ok, exsss}.
random_seed() -> {ok, {1,4,3}}.
shutdown_on_error() -> false.
