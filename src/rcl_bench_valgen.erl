-module(rcl_bench_valgen).

-export([new/2, dimension/2]).

%% ====================================================================
%% API
%% ====================================================================

new({fixed_bin, Size}, Id) when is_integer(Size), Size >= 0 ->
    Source = init_source(Id),
    fun () ->
        data_block(Source, Size)
    end;
new({fixed_bin, Size, Val}, _Id)
    when is_integer(Size), Size >= 0, is_integer(Val), Val >= 0, Val =< 255 ->
    Data = list_to_binary(lists:duplicate(Size, Val)),
    fun () ->
        Data
    end;
new({fixed_char, Size}, _Id) when is_integer(Size), Size >= 0 ->
    fun () ->
        list_to_binary(lists:map(fun (_) ->
            rand:uniform(95) + 31
                                 end,
            lists:seq(1, Size)))
    end;
%%new({exponential_bin, MinSize, Mean}, Id)
%%  when is_integer(MinSize), MinSize >= 0, is_number(Mean), Mean > 0 ->
%%    Source = init_source(Id),
%%    fun() -> data_block(Source, MinSize + trunc(basho_bench_stats:exponential(1 / Mean))) end;
new({uniform_bin, MinSize, MaxSize}, Id)
    when is_integer(MinSize), is_integer(MaxSize), MinSize < MaxSize ->
    Source = init_source(Id),
    Diff = MaxSize - MinSize,
    fun () ->
        data_block(Source, MinSize + rand:uniform(Diff))
    end;
new({function, Module, Function, Args}, Id)
    when is_atom(Module), is_atom(Function), is_list(Args) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            erlang:apply(Module, Function, [Id] ++ Args);
        _Error ->
            rcl_bench_util:exit("Could not find valgen function: ~p:~p\n", [Module, Function])
    end;
new({uniform_int, MaxVal}, _Id) when is_integer(MaxVal), MaxVal >= 1 ->
    fun () ->
        rand:uniform(MaxVal)
    end;
new(Other, _Id) ->
    rcl_bench_util:exit("Invalid value generator requested: ~p\n", [Other]).

dimension({fixed_bin, Size}, KeyDimension) ->
    Size * KeyDimension;
dimension(_Other, _) ->
    0.0.

%% ====================================================================
%% Internal Functions
%% ====================================================================

init_source(Id) ->
    BlobFile = application:get_env(rcl_bench, value_generator_blob_file, undefined),
    init_source(Id, BlobFile).

init_source(_Id, undefined) ->
    logger:debug("Random source"),
    BlobSourceSize = application:get_env(rcl_bench, value_generator_source_size, 1048576),
    {value_generator_source_size, BlobSourceSize, crypto:strong_rand_bytes(BlobSourceSize)};
init_source(_Id, Path) ->
    {Path, {ok, Bin}} = {Path, file:read_file(Path)},
    logger:debug("Path source: ~p", Path),
    {value_generator_blob_file, size(Bin), Bin}.

data_block({SourceCfg, SourceSz, Source}, BlockSize) ->
    case SourceSz - BlockSize > 0 of
        true ->
            Offset = rand:uniform(SourceSz - BlockSize),
            <<_:Offset/bytes, Slice:BlockSize/bytes, _Rest/binary>> = Source,
            Slice;
        false ->
            logger:warning("~p is too small ~p < ~p", [SourceCfg, SourceSz, BlockSize]),
            Source
    end.
