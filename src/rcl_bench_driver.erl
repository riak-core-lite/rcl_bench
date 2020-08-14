-module(rcl_bench_driver).

-export([new/1, run/4, terminate/2]).

-record(state, {url, existing = []}).

%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
    logger:notice("(~p) Initializing driver ~p", [node(), _Id]),
    {ok, Ip} = application:get_env(rcl_bench, ip),
    {ok, Port} = application:get_env(rcl_bench, port),

    Url = "http://" ++ atom_to_list(Ip) ++ ":" ++ integer_to_list(Port) ++ "/rclref/",

    {ok, #state{url = Url}}.

run(get, KeyGen, _ValueGen, #state{url = Url} = State) ->
    Key = KeyGen(),

    Url0 = list_to_binary(Url ++ integer_to_list(Key)),
    {ok, _, _, ClientRef} = hackney:request(get, Url0, [], <<>>, []),
    {ok, _} = hackney:body(ClientRef),

    {ok, State};
run(get_own_puts, _, _, #state{existing = []} = State) ->
    {ok, State};
run(get_own_puts, _KeyGen, _ValueGen, #state{url = Url, existing = Existing} = State) ->
    Max = length(Existing),
    Take = rand:uniform(Max),
    Key = lists:nth(Take, Existing),
    Url0 = list_to_binary(Url ++ integer_to_list(Key)),
    {ok, 200, _, ClientRef} = hackney:request(get, Url0, [], <<>>, []),
    {ok, _} = hackney:body(ClientRef),

    {ok, State};
run(put, KeyGen, ValueGen, #state{existing = Existing, url = Url} = State) ->
    Key = KeyGen(),
    Val = ValueGen(),

    Url0 = list_to_binary(Url ++ integer_to_list(Key)),

    {ok, 200, _, ClientRef} = hackney:request(post, Url0, [], Val, []),
    {ok, <<>>} = hackney:body(ClientRef),

    {ok, State#state{existing = Existing ++ [Key]}};
run(error, KeyGen, _ValueGen, State) ->
    _Key = KeyGen(),
    {error, went_wrong, State};
run(driver_error, KeyGen, _ValueGen, State) ->
    Key = KeyGen(),
    Key = 42,
    {error, went_wrong, State}.

terminate(_Reason, _B) ->
    ok.
