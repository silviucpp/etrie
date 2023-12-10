-module(integrity_test).

-include_lib("eunit/include/eunit.hrl").
-include("etrie.hrl").

data_type_mapping_test() ->
    {ok, T} = etrie:new(),

    IntVal = 1,
    UintVal = -1,
    Atom = true,
    Binary = <<"some text in binary">>,
    Float = 12344.4,
    FloatNeg = -4532.45,
    Other = ["1", "4", atom, 12],
    Other2 = {something, <<"ssd">>, ["1", "4", atom, 12]},

    ?assertEqual(ok, etrie:insert(T, <<"int">>, IntVal)),
    ?assertEqual(ok, etrie:insert(T, <<"uint">>, UintVal)),
    ?assertEqual(ok, etrie:insert(T, <<"atom">>, Atom)),
    ?assertEqual(ok, etrie:insert(T, <<"binary">>, Binary)),
    ?assertEqual(ok, etrie:insert(T, <<"float">>, Float)),
    ?assertEqual(ok, etrie:insert(T, <<"float_neg">>, FloatNeg)),
    ?assertEqual(ok, etrie:insert(T, <<"other">>, Other)),
    ?assertEqual(ok, etrie:insert(T, <<"othe2">>, Other2)),

    ?assertEqual({ok, IntVal}, etrie:lookup(T, <<"int">>)),
    ?assertEqual({ok, UintVal}, etrie:lookup(T, <<"uint">>)),
    ?assertEqual({ok, Atom}, etrie:lookup(T, <<"atom">>)),
    ?assertEqual({ok, Binary}, etrie:lookup(T, <<"binary">>)),
    ?assertEqual({ok, Float}, etrie:lookup(T, <<"float">>)),
    ?assertEqual({ok, FloatNeg}, etrie:lookup(T, <<"float_neg">>)),
    ?assertEqual({ok, Other}, etrie:lookup(T, <<"other">>)),
    ?assertEqual({ok, Other2}, etrie:lookup(T, <<"othe2">>)),

    ?assertEqual(ok, etrie:clear(T)),
    ok.

nested_trie_test() ->
    {ok, T} = etrie:new(),
    Pid = spawn(fun() ->
        {ok, T2} = etrie:from_list([{<<"apple">>, 1}]),
        ?assertEqual(ok, etrie:insert(T, <<"trie2">>, T2))
    end),

    ok = wait_process(Pid),
    {ok, Ref} = etrie:lookup(T, <<"trie2">>),
    ?assertEqual({ok, 1}, etrie:lookup(Ref, <<"apple">>)).

basic_ops_test() ->
    {ok, T} = etrie:new(),
    ?assertEqual(0, etrie:size(T)),
    ?assertEqual(true, etrie:is_empty(T)),

    ?assertEqual(ok, etrie:insert(T, <<"one">>, 1)),
    ?assertEqual(ok, etrie:insert(T, <<"two">>, 2)),
    ?assertEqual(ok, etrie:insert(T, <<"three">>, 3)),
    ?assertEqual(ok, etrie:insert(T, <<"four">>, 4)),

    ?assertEqual( {ok,[{<<"four">>,4}, {<<"two">>,2}, {<<"three">>,3}, {<<"one">>,1}]}, etrie:to_list(T)),

    ?assertEqual({ok, 4}, etrie:lookup(T, <<"four">>)),
    ?assertEqual(null, etrie:lookup(T, <<"N/A">>)),
    ?assertEqual({ok,[{<<"two">>,2},{<<"three">>,3}]}, etrie:lookup_range(T, <<"t">>)),
    ?assertEqual({ok,[]}, etrie:lookup_range(T, <<"N/A">>)),

    ?assertEqual(4, etrie:size(T)),
    ?assertEqual(false, etrie:is_empty(T)),

    ?assertEqual({ok, true}, etrie:is_member(T, <<"one">>)),
    ?assertEqual({ok, false}, etrie:is_member(T, <<"ONE">>)),

    ?assertEqual({ok, 1}, etrie:remove(T, <<"one">>)),
    ?assertEqual({ok, 0}, etrie:remove(T, <<"ONE">>)),

    ?assertEqual(3, etrie:size(T)),

    ?assertEqual({ok, 2}, etrie:remove_prefix(T, <<"t">>)),
    ?assertEqual({ok, 0}, etrie:remove_prefix(T, <<"T">>)),

    ?assertEqual(1, etrie:size(T)),

    {ok, T2} = etrie:from_list([{<<"apple">>, 1}, {<<"mango">>, 2}, {<<"apricot">>, 3}, {<<"mandarin">>, 4}, {<<"melon">>, 5}, {<<"macadamia">>, 6}]),
    ?assertEqual({ok, <<"apple">>, 1}, etrie:longest_prefix(T2, <<"apple juice">>)),
    ?assertEqual(6, etrie:size(T2)),
    ?assertEqual(ok, etrie:clear(T2)),
    ok.

insert_with_too_long_string_test() ->
    {ok, T} = etrie:new(),
    ok = lists:foreach(fun(X) -> ?assertEqual(ok, etrie:insert(T, integer_to_binary(X), X)) end, lists:seq(0, 999)),
    ?assertEqual(ok, etrie:set_burst_threshold(T, 8)),

    ok = lists:foreach(fun(X) -> ?assertEqual(ok, etrie:insert(T, integer_to_binary(X), X)) end, lists:seq(0, 999)),
    ?assertEqual(1000, etrie:size(T)),

    LongString0 = build_binary($a, ?MAX_KEY_SIZE),
    ?assertEqual(?MAX_KEY_SIZE, byte_size(LongString0)),

    ?assertEqual(ok, etrie:insert(T, LongString0, ?MAX_KEY_SIZE)),

    LongString1 = build_binary($a, ?MAX_KEY_SIZE+1),
    ?assertEqual({error,<<"Key is too long.">>}, etrie:insert(T, LongString1, ?MAX_KEY_SIZE+1)),
    ok.

longest_prefix_test() ->
    {ok, T} = etrie:from_list([
        {<<"a">>, a},
        {<<"aa">>, aa},
        {<<"aaa">>, aaa},
        {<<"aaaaa">>, aaaa},
        {<<"aaaaaa">>, aaaaaa},
        {<<"aaaaaaa">>, aaaaaaa},
        {<<"ab">>, ab},
        {<<"abcde">>, abcde},
        {<<"abcdf">>, abcdf},
        {<<"abcdg">>, abcdg},
        {<<"abcdh">>, abcdh},
        {<<"babc">>, babc}
    ]),

    ?assertEqual({ok, <<"a">>, a}, etrie:longest_prefix(T, <<"a">>)),
    ?assertEqual({ok, <<"aa">>, aa}, etrie:longest_prefix(T, <<"aa">>)),
    ?assertEqual({ok, <<"aaa">>, aaa}, etrie:longest_prefix(T, <<"aaa">>)),
    ?assertEqual({ok, <<"aaa">>, aaa}, etrie:longest_prefix(T, <<"aaaa">>)),
    ?assertEqual({ok, <<"ab">>, ab}, etrie:longest_prefix(T, <<"ab">>)),
    ?assertEqual({ok, <<"ab">>, ab}, etrie:longest_prefix(T, <<"abc">>)),
    ?assertEqual({ok, <<"ab">>, ab}, etrie:longest_prefix(T, <<"abcd">>)),
    ?assertEqual({ok, <<"ab">>, ab}, etrie:longest_prefix(T, <<"abcdz">>)),
    ?assertEqual({ok, <<"abcde">>, abcde}, etrie:longest_prefix(T, <<"abcde">>)),
    ?assertEqual({ok, <<"abcde">>, abcde}, etrie:longest_prefix(T, <<"abcdef">>)),
    ?assertEqual({ok, <<"abcde">>, abcde}, etrie:longest_prefix(T, <<"abcdefg">>)),

    ?assertEqual(null, etrie:longest_prefix(T, <<"dabc">>)),
    ?assertEqual(null, etrie:longest_prefix(T, <<"b">>)),
    ?assertEqual(null, etrie:longest_prefix(T, <<"bab">>)),
    ?assertEqual(null, etrie:longest_prefix(T, <<"babd">>)),
    ?assertEqual(null, etrie:longest_prefix(T, <<"">>)),

    ?assertEqual(ok, etrie:insert(T, <<>>, 1)),
    ?assertEqual({ok, <<"">>, 1}, etrie:longest_prefix(T, <<"dabc">>)),
    ?assertEqual({ok, <<"">>, 1}, etrie:longest_prefix(T, <<"">>)),
    ok.

% internals

build_binary(Char, MaxSize) when is_integer(MaxSize), MaxSize >= 0 ->
    <<Char:MaxSize/unit:8>>.

wait_process(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        _ ->
            wait_process(Pid)
    end.
