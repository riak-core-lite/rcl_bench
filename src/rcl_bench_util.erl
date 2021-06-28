-module(rcl_bench_util).

%% API
-export([
    user_friendly_bytes/1, 
    exit/2, 
    now_epoch/0,
    now_epoch/1,
    now_epoch_micro/0,
    normalize_label/1
]).

%%
%% Convert a number of bytes into a more user-friendly representation
%%
user_friendly_bytes(Size) ->
    lists:foldl(fun (Desc, {Sz, SzDesc}) ->
                        case Sz > 1000 of
                            true ->
                                {Sz / 1024, Desc};
                            false ->
                                {Sz, SzDesc}
                        end
                end,
                {Size, bytes},
                ['KB', 'MB', 'GB']).

exit(Message, Args) ->
    logger:error(Message, Args),
    init:stop().

% ==========
% file utils
% ==========

normalize_label(Label) when is_list(Label) ->
    replace_special_chars(Label);
normalize_label(Label) when is_binary(Label) ->
    normalize_label(binary_to_list(Label));
normalize_label(Label) when is_integer(Label) ->
    normalize_label(integer_to_list(Label));
normalize_label(Label) when is_atom(Label) ->
    normalize_label(atom_to_list(Label));
normalize_label(Label) when is_tuple(Label) ->
    Parts = [normalize_label(X) || X <- tuple_to_list(Label)],
    string:join(Parts, "-").

replace_special_chars([H | T])
    when H >= $0 andalso H =< $9 orelse
             H >= $A andalso H =< $Z orelse H >= $a andalso H =< $z ->
    [H | replace_special_chars(T)];
replace_special_chars([_ | T]) ->
    [$- | replace_special_chars(T)];
replace_special_chars([]) ->
    [].


now_epoch() ->
    now_epoch(os:timestamp()).

now_epoch({Mega, Sec, _}) ->
    (Mega * 1000000 + Sec).

now_epoch_micro() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.