-module(rcl_bench_keygen).

-export([new/2, dimension/1]).

%%       ,sequential_int_generator/4

%% Use a fixed shape for Pareto that will yield the desired 80/20
%% ratio of generated values.
-define(PARETO_SHAPE, 1.5).

%% ====================================================================
%% API
%% ====================================================================

new({biased_partial, MaxKey, ReplicationFactor, PercentageExternal, NumDcs, IdDc}, _Id) ->
    KeySpace = MaxKey div NumDcs,
    RangeHere = ReplicationFactor,
    MinHere =
        case IdDc - (ReplicationFactor - 1) of
            Val when Val < 1 ->
                NumDcs - Val;
            Val2 ->
                Val2
        end,
    MinNotHere =
        case (IdDc + 1) rem NumDcs of
            0 ->
                NumDcs;
            Oth ->
                Oth
        end,

    RangeNotHere =
        case NumDcs - ReplicationFactor of
            0 ->
                1;
            Oth2 ->
                Oth2
        end,
    fun () ->
            DcNum =
                case rand:uniform() > PercentageExternal of
                    false ->
                        case (MinNotHere + (rand:uniform(RangeNotHere) - 1)) rem NumDcs of
                            0 ->
                                NumDcs;
                            Other ->
                                Other
                        end;
                    true ->
                        case (MinHere + (rand:uniform(RangeHere) - 1)) rem NumDcs of
                            0 ->
                                NumDcs;
                            Other ->
                                Other
                        end
                end,
            rand:uniform(KeySpace) * NumDcs + DcNum
    end;
new({int_to_bin_bigendian, InputGen}, Id) ->
    Gen = new(InputGen, Id),
    fun () ->
            <<(Gen()):32/big>>
    end;
new({int_to_bin_littleendian, InputGen}, Id) ->
    Gen = new(InputGen, Id),
    fun () ->
            <<(Gen()):32/little>>
    end;
new({int_to_str, InputGen}, Id) ->
    Gen = new(InputGen, Id),
    fun () ->
            integer_to_list(Gen())
    end;
new({to_binstr, FmtStr, InputGen}, Id) ->
    Gen = new(InputGen, Id),
    fun () ->
            list_to_binary(io_lib:format(FmtStr, [Gen()]))
    end;
new({base64, InputGen}, Id) ->
    Gen = new(InputGen, Id),
    fun () ->
            base64:encode(Gen())
    end;
new({concat_binary, OneGen, TwoGen}, Id) ->
    Gen1 = new(OneGen, Id),
    Gen2 = new(TwoGen, Id),
    fun () ->
            <<(Gen1())/binary, (Gen2())/binary>>
    end;
%%new({sequential_int, MaxKey}, Id)
%%  when is_integer(MaxKey), MaxKey > 0 ->
%%    Ref = make_ref(),
%%    DisableProgress =
%%        basho_bench_config:get(disable_sequential_int_progress_report, false),
%%    fun() -> sequential_int_generator(Ref, MaxKey, Id, DisableProgress) end;
%%new({partitioned_sequential_int, MaxKey}, Id) ->
%%    new({partitioned_sequential_int, 0, MaxKey}, Id);
%%new({partitioned_sequential_int, StartKey, NumKeys}, Id)
%%  when is_integer(StartKey), is_integer(NumKeys), NumKeys > 0 ->
%%    Workers = basho_bench_config:get(concurrent),
%%    Range = NumKeys div Workers,
%%    MinValue = StartKey + Range * (Id - 1),
%%    MaxValue = StartKey +
%%               % Last worker picks up remainder to include entire range
%%               case Workers == Id of true-> NumKeys; false -> Range * Id end,
%%    Ref = make_ref(),
%%    DisableProgress =
%%        basho_bench_config:get(disable_sequential_int_progress_report, false),
%%    ?LOG_DEBUG("ID ~p generating range ~p to ~p\n", [Id, MinValue, MaxValue]),
%%    fun() -> sequential_int_generator(Ref, MaxValue - MinValue, Id, DisableProgress) + MinValue end;
new({uniform_int, MaxKey}, _Id) when is_integer(MaxKey), MaxKey > 0 ->
    fun () ->
            rand:uniform(MaxKey)
    end;
new({uniform_int, StartKey, NumKeys}, _Id)
    when is_integer(StartKey), is_integer(NumKeys), NumKeys > 0 ->
    fun () ->
            rand:uniform(NumKeys) + StartKey - 1
    end;
new({pareto_int, MaxKey}, _Id)
  when is_integer(MaxKey), MaxKey > 0 ->
    pareto(trunc(MaxKey * 0.2), ?PARETO_SHAPE);
%%new({dc_bias, NumDCs, DcId, NodesPerDC, MaxKey}, _Id) ->
%%    Max = MaxKey * NodesPerDC,
%%    bias(NumDCs, DcId, Max, trunc(Max * 0.2), ?PARETO_SHAPE);
%%new({truncated_pareto_int, MaxKey}, Id) ->
%%    Pareto = new({pareto_int, MaxKey}, Id),
%%    fun() -> erlang:min(MaxKey, Pareto()) end;
%%new(uuid_v4, _Id) ->
%%    fun() -> uuid:v4() end;
%%new({function, Module, Function, Args}, Id)
%%  when is_atom(Module), is_atom(Function), is_list(Args) ->
%%    case code:ensure_loaded(Module) of
%%        {module, Module} ->
%%            erlang:apply(Module, Function, [Id] ++ Args);
%%        _Error ->
%%            ?FAIL_MSG("Could not find keygen function: ~p:~p\n", [Module, Function])
%%    end;
%% Adapt a value generator. The function keygen would work if Id was added as
%% the last parameter. But, alas, it is added as the first.
%%new({valgen, ValGen}, Id) ->
%%    basho_bench_valgen:new(ValGen, Id);
new(Bin, _Id) when is_binary(Bin) ->
    fun () ->
            Bin
    end;
new(Other, _Id) ->
    rcl_bench_util:exit("Invalid key generator requested: ~p\n", [Other]).

dimension({int_to_str, InputGen}) ->
    dimension(InputGen);
dimension({int_to_bin, InputGen}) ->
    dimension(InputGen);
dimension({to_binstr, _FmtStr, InputGen}) ->
    dimension(InputGen);
dimension({base64, InputGen}) ->
    dimension(InputGen);
dimension({concat_binary, OneGen, TwoGen}) ->
    erlang:min(dimension(OneGen), dimension(TwoGen));
dimension({sequential_int, MaxKey}) ->
    MaxKey;
dimension({partitioned_sequential_int, MaxKey}) ->
    MaxKey;
dimension({uniform_int, MaxKey}) ->
    MaxKey;
dimension({truncated_pareto_int, MaxKey}) ->
    MaxKey;
dimension(Bin) when is_binary(Bin) ->
    undefined;
dimension(Other) ->
    logger:warning("No dimension available for key generator: ~p", [Other]),
    undefined.

%% ====================================================================
%% Internal functions
%% ====================================================================

pareto(Mean, Shape) ->
    S1 = (-1 / Shape),
    S2 = Mean * (Shape - 1),
    fun() ->
            U = 1 - rand:uniform(),
            trunc((math:pow(U, S1) - 1) * S2)
    end.


%%bias(NumDCs, DcId, Max, Mean, Shape) ->
%%    DcRange = (Max div NumDCs) * (1 - DcId),
%%    S1 = (-1 / Shape),
%%    S2 = Mean * (Shape - 1),
%%    fun() ->
%%            U = 1 - rand:uniform(),
%%            Key1 = erlang:min(Max, trunc((math:pow(U, S1) - 1) * S2)),
%%            (Key1 + DcRange) rem Max
%%    end.
%%
%%
%%
%%sequential_int_generator(Ref, MaxValue, Id, DisableProgress) ->
%%   %% A bit of evil here. We want to generate numbers in sequence and stop
%%   %% at MaxKey. This means we need state in our anonymous function. Use the process
%%   %% dictionary to keep track of where we are.
%%   case erlang:get({sigen, Ref}) of
%%       undefined ->
%%           seq_gen_put(Ref, seq_gen_read_resume_value(Id, MaxValue)),
%%           sequential_int_generator(Ref, MaxValue, Id, DisableProgress);
%%       MaxValue ->
%%           throw({stop, empty_keygen});
%%       Value ->
%%           case Value rem 500 of
%%               400 -> ok = seq_gen_write_resume_value(Id, Value);
%%                 _ -> ok
%%           end,
%%           case (not DisableProgress) andalso Value rem 5000 == 0 of
%%               true ->
%%                   Me = self(),
%%                   spawn(fun() -> ?LOG_DEBUG("sequential_int_gen: ~p: ~p (~w%)\n", [Me, Value, trunc(100 * (Value / MaxValue))]) end);
%%               false ->
%%                   ok
%%           end,
%%           seq_gen_put(Ref, Value+1),
%%           Value
%%   end.
%%
%%seq_gen_put(Ref, Value) ->
%%    erlang:put({sigen, Ref}, Value).
%%
%%seq_gen_write_resume_value(Id, Value) ->
%%    case seq_gen_state_dir(Id) of
%%        "" ->
%%            ok;
%%        Path ->
%%            OutFile = Path ++ "/" ++ integer_to_list(Id),
%%            OutFileTmp = Path ++ "/" ++ integer_to_list(Id) ++ ".tmp",
%%            Bin = term_to_binary(Value),
%%            ok = file:write_file(OutFileTmp, Bin),
%%            {ok, Bin} = file:read_file(OutFileTmp),
%%            ok = file:rename(OutFileTmp, OutFile)
%%    end.
%%
%%seq_gen_read_resume_value(Id, MaxValue) ->
%%    case seq_gen_state_dir(Id) of
%%        "" ->
%%            0;
%%        Path ->
%%            try
%%                InFile = Path ++ "/" ++ integer_to_list(Id),
%%                {ok, Bin} = file:read_file(InFile),
%%                Value = binary_to_term(Bin),
%%            case Value > MaxValue of
%%               true ->
%%                  ?LOG_WARNING("Id ~p resume value ~p exceeds maximum value ~p. Restarting from 0",[Id, Value, MaxValue]),
%%                  0;
%%               false ->
%%                  ?LOG_DEBUG("Id ~p resuming from value ~p\n", [Id, Value]),
%%                  Value
%%            end
%%            catch
%%                error:{badmatch, _} ->
%%                    0;
%%                X:Y ->
%%                    ?LOG_DEBUG("Error reading resume value for Id ~p ~p: ~p ~p\n",
%%                           [Id, Path ++ "/" ++ integer_to_list(Id), X, Y]),
%%                    0
%%            end
%%    end.
%%
%%seq_gen_state_dir(Id) ->
%%    Key = sequential_int_state_dir,
%%    DirValid = get(seq_dir_test_res),
%%    case {basho_bench_config:get(Key, "") , DirValid} of
%%        {_Dir, false} ->
%%            "";
%%        {[$/|_] = Dir, true} ->
%%            Dir;
%%        {[$/|_] = Dir, undefined} ->
%%            case filelib:ensure_dir(filename:join(Dir, "touch")) of
%%                ok ->
%%                    put(seq_dir_test_res, true),
%%                    Dir;
%%                MkDirErr ->
%%                    ?LOG_WARNING("Could not ensure ~p -> ~p was a writable dir: ~p", [Key, Dir, MkDirErr]),
%%                    put(seq_dir_test_res, false),
%%                    put(you_have_been_warned, true),
%%                    ""
%%            end;
%%        {Else, _} ->
%%            case Else /= "" andalso
%%                 get(you_have_been_warned) == undefined andalso Id == 1 of
%%                true ->
%%                    ?LOG_WARNING("Config value ~p -> ~p is not an absolute "
%%                          "path, ignoring!\n", [Key, Else]),
%%                    put(you_have_been_warned, true);
%%                false ->
%%                    ok
%%            end,
%%            ""
%%    end.
%%
%%reset_sequential_int_state() ->
%%    case [X || {{sigen, X}, _} <- element(2, process_info(self(),
%%                                                          dictionary))] of
%%        [Ref] ->
%%            erlang:put({sigen, Ref}, 0);
%%        [] ->
%%            ok
%%    end.
