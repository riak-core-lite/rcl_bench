-module(rkv_driver).
-behaviour(rcl_bench_driver).

% dependencies: hackney

-export([new/1, run/4, terminate/2]).

-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
         key_generator/0, value_generator/0, random_algorithm/0,
         random_seed/0, shutdown_on_error/0]).

new(Id) ->
    Node = list_to_atom("rkv@127.0.0.1"),
    State = #{id => Id, node => Node, existing => #{}, mod => 'Elixir.Rkv'},
    {ok, State}.

run(get, KeyGen, _ValueGen, #{node := Node, mod := Mod} = State) ->
    Key = KeyGen(),
    {_, _} = rpc:call(Node, Mod, get, [Key]),
    {ok, State};
run(put, KeyGen, ValueGen, #{existing := Existing, node := Node, mod := Mod} = State) ->
    Key = KeyGen(),
    Value = ValueGen(),
    ok = rpc:call(Node, Mod, put, [Key, Value]),
    {ok, State#{existing := Existing#{Key => true}}};
run(get_own_puts, _KeyGen, _ValueGen, #{existing := Existing} = State)
  when map_size(Existing) =:= 0 ->
    {ok, State};
run(get_own_puts, _KeyGen, _ValueGen, #{existing := Existing, node := Node, mod := Mod} = State) ->
    Max = maps:size(Existing),
    Take = rand:uniform(Max),
    {Key, _} = lists:nth(Take, maps:to_list(Existing)),
    {ok, _} = rpc:call(Node, Mod, get, [Key]),
    {ok, State}.

terminate(_, _) -> ok.

% config callbacks

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
