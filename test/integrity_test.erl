-module(integrity_test).

-include_lib("eunit/include/eunit.hrl").
-include("etrie.hrl").

basic_ops_test() ->
    {ok, T} = etrie:new(),
    ?assertEqual(0, etrie:size(T)),
    ?assertEqual(true, etrie:is_empty(T)),

    ?assertEqual(ok, etrie:insert(T, <<"one">>, 1)),
    ?assertEqual(ok, etrie:insert(T, <<"two">>, 2)),
    ?assertEqual(ok, etrie:insert(T, <<"three">>, 3)),
    ?assertEqual(ok, etrie:insert(T, <<"four">>, 4)),

    ?assertEqual(4, etrie:size(T)),
    ?assertEqual(false, etrie:is_empty(T)),

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

test_longest_prefix_test() ->
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
