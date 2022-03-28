-module(antidote_driver).
-behaviour(rcl_bench_driver).

-export([new/1, run/4, terminate/2]).

-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
         key_generator/0, value_generator/0, random_algorithm/0,
         random_seed/0, shutdown_on_error/0, crash_is_recoverable/0]).
% generic config 
mode() -> {ok, {rate, 5}}.
concurrent_workers() -> {ok, 1}.
duration() -> {ok, 1}.
operations() -> {ok, [{txn, 1}]}.
%operations() -> {ok, [{txn, 1}]}.
test_dir() -> {ok, "tests"}.
key_generator() -> {ok, {uniform_int, 100000}}.
value_generator() -> {ok, {fixed_bin, 100}}.
random_algorithm() -> {ok, exsss}.
random_seed() -> {ok, {1,4,3}}.
shutdown_on_error() -> false.
crash_is_recoverable() -> true.

% antidote config 
%%{operations, [{update_only_txn, 1}, {read_only_txn, 1}, {append, 1}, {read, 1}, {txn, 1} ]}.
antidote_pb_port() -> 8087.
antidote_pb_ip() -> '127.0.0.1'.
antidote_types() -> dict:from_list([{antidote_crdt_counter_pn, [{increment,1}, {decrement,1}]}]).
%%{antidote_types, [{antidote_crdt_set_aw, [add, remove]}]}.
%%{antidote_types, [{antidote_crdt_set_go, [add, remove]}]}.
%%{antidote_types, [{antidote_crdt_register_mv, [assign]}]}.
%%{antidote_types, [{antidote_crdt_register_lww, [assign]}]}.
%% Use the following parameter to set the size of the orset
set_size() -> 10.


%%%%%%%%%%%%%%%%%%%%%%%%%%%% for transactions %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The following parameters are used when issuing transactions.
%% When running append and read operations, they are ignored.

%% Number of reads; update_only_txn ignores it.
num_reads() -> 10.
%% Number of updates; read_only_txn ignores it.
num_updates() -> 10.

%% If sequential_reads is set to true,
%% the client will send each read (of a total
%% num_reads) in a different antidote:read_objects call.
%% when set to false, all (num_reads) reads will be sent
%% in a single read_objects call, which is faster, as
%% antidote will process them in parallel.
sequential_reads() -> false.

%% Idem for updates
-spec sequential_writes() -> false | true.
sequential_writes() -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% end for transactions %%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(BUCKET, <<"antidote_bench_bucket">>).

-record(state, {worker_id,
                time,
                pb_pid,
                commit_time,
                num_reads,
                num_updates
                }).

new(Id) ->
  io:format(user, "~nInitializing antidote bench worker~n=====================~n", []),

  io:format(user, "Using target node ~p for worker ~p", [antidote_pb_ip(), Id]),

  {ok, Pid} = antidotec_pb_socket:start_link(antidote_pb_ip(), antidote_pb_port()),
  io:format(user, "Connection established", []),

  % some sanity checks
  true = num_reads() > 0,
  true = num_updates() > 0,

  {ok, #state{
    worker_id = Id,
    time = {1, 1, 1},
    pb_pid = Pid,
    commit_time = term_to_binary(ignore),
    num_reads = num_reads(),
    num_updates = num_updates()
  }}.


%% @doc This transaction will only perform update operations,
%% by calling the static update_objects interface of antidote.
%% the number of operations is defined by the {num_updates, x}
%% parameter in the config file.
run(update_only_txn, _KeyGen, _ValueGen, State=#state{num_updates = NumUpdates}) when NumUpdates =< 0 ->
  {ok, State};
run(update_only_txn, KeyGen, ValueGen, State=#state{pb_pid=Pid, commit_time=OldCommitTime, num_updates = NumUpdates})->
  {ok, TxId} = antidotec_pb:start_transaction(Pid, OldCommitTime, [{static, true}]),
  UpdateIntKeys = generate_keys(NumUpdates, KeyGen),
  BObjs = multi_get_random_param_new(UpdateIntKeys, antidote_types(), ValueGen(), undefined, set_size()),
  ok = create_update_operations(Pid, BObjs, TxId, sequential_writes()),
  {ok, BCommitTime} = antidotec_pb:commit_transaction(Pid, TxId),
  {ok, State#state{commit_time=BCommitTime}};

%% @doc This transaction will only perform read operations in
%% an antidote's read/only transaction.
%% the number of operations is defined by the {num_reads, x}
%% parameter in the config file.
run(read_only_txn, _KeyGen, _ValueGen, State=#state{num_reads = NumReads}) when NumReads =< 0 ->
  {ok, State};
run(read_only_txn, KeyGen, _ValueGen, State=#state{pb_pid=Pid, commit_time = OldCommitTime}) ->
  {ok, TxId} = antidotec_pb:start_transaction(Pid, OldCommitTime, [{static, true}]),
  IntegerKeys = generate_keys(num_reads(), KeyGen),
  BoundObjects = [{list_to_binary(integer_to_list(K)), get_key_type(K, antidote_types()), ?BUCKET} || K <- IntegerKeys],
  {ok, _} = create_read_operations(Pid, BoundObjects, TxId, sequential_reads()),
  {ok, BCommitTime} = antidotec_pb_socket:get_last_commit_time(Pid),
  {ok, State#state{commit_time = BCommitTime}};

%% @doc A general transaction.
%% it first performs reads to a number of objects defined by the
%% {num_reads, X} parameter in the config file.
%% Then, it updates {num_updates, X}.
run(txn, KeyGen, ValueGen, State=#state{pb_pid=Pid, worker_id=_Id, commit_time=OldCommitTime
  , num_reads = NumReads, num_updates = NumUpdates }) ->

  {ok, TxId} = antidotec_pb:start_transaction(Pid, OldCommitTime, [{static, false}]),

  {_ReadResult, IntKeys} = case NumReads > 0 of
                            true ->
                              IntegerKeys = generate_keys(NumReads, KeyGen),
                              BoundObjects = [{list_to_binary(integer_to_list(K)), get_key_type(K, antidote_types()), ?BUCKET} || K <- IntegerKeys],
                              {ok, RS} = create_read_operations(Pid, BoundObjects, TxId, sequential_reads()),
                              {RS, IntegerKeys};
                            false ->
                              {no_reads, no_reads}
                          end,

  UpdateIntKeys = case IntKeys of
                    no_reads ->
                      %% write only transaction
                      generate_keys(NumUpdates, KeyGen);
                    _ ->
                      %% The following selects the latest reads for updating.
                      lists:sublist(IntKeys, NumReads - NumUpdates + 1, NumUpdates)
                  end,

  BObjs = multi_get_random_param_new(UpdateIntKeys, antidote_types(), ValueGen(), undefined, set_size()),
  ok = create_update_operations(Pid, BObjs, TxId, sequential_writes()),
  {ok, BCommitTime} = antidotec_pb:commit_transaction(Pid, TxId),
  {ok, State#state{commit_time = BCommitTime}};

%% @doc the append command will run a transaction with a single update, and no reads.
run(append, KeyGen, ValueGen, State) ->
  run(txn, KeyGen, ValueGen, State#state{num_reads=0,num_updates=1});

%% @doc the read command will run a transaction with a single read, and no updates.
run(read, KeyGen, ValueGen, State) ->
  run(txn, KeyGen, ValueGen, State#state{num_reads=1,num_updates=0}).

terminate(_, _) -> ok.




create_read_operations(Pid, BoundObjects, TxInfo, IsSeq) ->
  case IsSeq of
    true->
      Result = lists:map(fun(BoundObj)->
        {ok, [Value]} = antidotec_pb:read_objects(Pid, [BoundObj], TxInfo),
        Value
                         end,BoundObjects),
      {ok, Result};
    false ->
      antidotec_pb:read_objects(Pid, BoundObjects, TxInfo)
  end.

create_update_operations(_Pid, [], _TxInfo, _IsSeq) ->
  ok;
create_update_operations(Pid, BoundObjects, TxInfo, IsSeq) ->
  case IsSeq of
    true ->
      lists:map(fun(BoundObj) ->
        antidotec_pb:update_objects(Pid, [BoundObj], TxInfo)
                end, BoundObjects),
      ok;
    false ->
      antidotec_pb:update_objects(Pid, BoundObjects, TxInfo)
  end.

%% @doc generate NumReads unique keys using the KeyGen
generate_keys(NumKeys, KeyGen) ->
  Seq = lists:seq(1, NumKeys),
  S = lists:foldl(fun(_, Set) ->
    N = unikey(KeyGen, Set),
    sets:add_element(N, Set)
                  end, sets:new(), Seq),
  sets:to_list(S).

unikey(KeyGen, Set) ->
  R = KeyGen(),
  case sets:is_element(R, Set) of
    true ->
      unikey(KeyGen, Set);
    false ->
      R
  end.

get_key_type(Key, Dict) ->
  Keys = dict:fetch_keys(Dict),
  RanNum = Key rem length(Keys),
  lists:nth(RanNum+1, Keys).

multi_get_random_param_new(KeyList, Dict, Value, Objects, SetSize) ->
  multi_get_random_param_new(KeyList, Dict, Value, Objects, SetSize, []).

multi_get_random_param_new([], _Dict, _Value, _Objects, _SetSize, Acc)->
  Acc;
multi_get_random_param_new([Key|Rest], Dict, Value, Objects, SetSize, Acc)->
  Type = get_key_type(Key, Dict),
  case Objects of
    undefined ->
      Obj = undefined,
      ObjRest = undefined;
    [H|T] ->
      Obj = H,
      ObjRest = T
  end,
  [Param] = get_random_param_new(Key, Dict, Type, Value, Obj, SetSize),
  multi_get_random_param_new(Rest, Dict, Value, ObjRest, SetSize, [Param|Acc]).

get_random_param_new(Key, Dict, Type, Value, Obj, SetSize)->
  Params=dict:fetch(Type, Dict),
  Num=rand:uniform(length(Params)),
  BKey=list_to_binary(integer_to_list(Key)),
  NewVal=case Value of
           Value when is_integer(Value)->
             integer_to_list(Value);
           Value when is_binary(Value)->
             Value
         end,
  case Type of
    antidote_crdt_counter_pn->
      case lists:nth(Num, Params) of
        {increment, Ammount}->
          [{{BKey, Type, ?BUCKET}, increment, Ammount}];
        {decrement, Ammount}->
          [{{BKey, Type, ?BUCKET}, decrement, Ammount}];
        increment->
          [{{BKey, Type, ?BUCKET}, increment, 1}];
        decrement->
          [{{BKey, Type, ?BUCKET}, decrement, 1}]
      end;

    RegisterType when ((RegisterType==antidote_crdt_register_mv) orelse (RegisterType==antidote_crdt_register_lww))->
      [{{BKey, Type, ?BUCKET}, assign, NewVal}];

    SetType when ((SetType==antidote_crdt_set_aw) orelse (SetType==antidote_crdt_set_go))->
      Set=
        case Obj of
          undefined->
            [];
          _ ->
            antidotec_set:value(Obj)
        end,
      %%Op = lists:nth(Num, Params),
      NewOp=case length(Set)=<SetSize of
              true->
                add;
              false->
                remove
            end,
      case NewOp of
        remove->
          case Set of
            []->
              [{{BKey, Type, ?BUCKET}, add_all, [NewVal]}];
            _ ->
              [{{BKey, Type, ?BUCKET}, remove_all, [lists:nth(rand:uniform(length(Set)), Set)]}]
          end;
        _->
          [{{BKey, Type, ?BUCKET}, add_all, [NewVal]}]
      end
  end.

