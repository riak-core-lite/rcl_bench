-module(rcl_bench_driver).

-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
    key_generator/0, value_generator/0, random_algorithm/0, random_seed/0,
    shutdown_on_error/0]).

-type state() :: term().
-type command() :: term().
-type keygen() :: term().
-type valuegen() :: term().
-type error() :: {error, term(), state()}.
-type mode_config() :: term().
-type key_generator_config() :: term().
-type value_generator_config() :: term().
-type random_algorithm_config() :: term().
-type random_seed_value() :: term().

-callback new(term()) -> {ok, state()}.
-callback run(command(), keygen(), valuegen(), state()) ->
    {ok | silent, state()}
    | {'stop', term()}
    | error().
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

%% {ok, {rate, integer() | max}}:
%% How often a thread should send a request.
%% Use 'max' for benchmarking.
%% If you are debugging, maybe {rate, N} can be useful,
%% which means each thread sends N requests per second.
mode() ->
    {ok, {rate, max}}.

%% Number of concurrent worker threads
%% This has to be carefully chosen.
%% Too few threads will not exploit the real throughput of the system,
%% while too high concurrency will over-stress the system and trigger timeouts.
concurrent_workers() ->
    {ok, 2}.

%% Test duration in minutes
duration() ->
    {ok, 1}.

%% Available operations and associated mix
%% Every operation command has to be implemented in the driver as a run callback
operations() ->
    {ok, [{get_own_puts, 3}, {put, 10}, {get, 2}]}.

%% Base test output directory
test_dir() ->
    {ok, "tests"}.

%% Key generators
%% {uniform_int, N} - Choose a uniformly distributed integer between 0 and N
key_generator() ->
    {ok, {uniform_int, 100000}}.

%% Value generators
%% {fixed_bin, N} - Fixed size binary blob of N bytes
value_generator() ->
    {ok, {fixed_bin, 100}}.

%% Which random algorithm to use
%% See https://erlang.org/doc/man/rand.html
random_algorithm() ->
    {ok, exsss}.

%% Seed guarantees that the random values are generated in the same order across
%% different runs for reproducability
random_seed() ->
    {ok, {1, 4, 3}}.

%% Shut down benchmark if one error occurs
shutdown_on_error() ->
    false.
