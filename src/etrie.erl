-module(etrie).

-export([
    new/0,
    new/1,
    from_list/1,
    from_list/2,
    insert/3,
    is_empty/1,
    size/1,
    get_burst_threshold/1,
    set_burst_threshold/2,
    clear/1,
    longest_prefix/2
]).

-spec new() ->
    {ok, reference()} | {error, any()}.

new() ->
    etrie_nif:new([]).

-spec new(non_neg_integer()) ->
    {ok, reference()} | {error, any()}.

new(BurstThreshold) ->
    etrie_nif:new([], BurstThreshold).

-spec from_list([{binary(), term()}]) ->
    {ok, reference()} | {error, any()}.

from_list(Items) ->
    etrie_nif:new(Items).

-spec from_list([{binary(), term()}], non_neg_integer()) ->
    {ok, reference()} | {error, any()}.

from_list(Items, BurstThreshold) ->
    etrie_nif:new(Items, BurstThreshold).

-spec insert(reference(), binary(), term()) ->
    ok | {error, any()}.

insert(Ref, Key, Value) ->
    etrie_nif:insert(Ref, Key, Value).

-spec is_empty(reference()) ->
    boolean().

is_empty(Ref) ->
    etrie_nif:is_empty(Ref).

-spec size(reference()) ->
    non_neg_integer().

size(Ref) ->
    etrie_nif:size(Ref).

-spec get_burst_threshold(reference()) ->
    non_neg_integer().

get_burst_threshold(Ref) ->
    etrie_nif:get_burst_threshold(Ref).

-spec set_burst_threshold(reference(), non_neg_integer()) ->
    ok | {error, any()}.

set_burst_threshold(Ref, BurstThreshold) ->
    etrie_nif:set_burst_threshold(Ref, BurstThreshold).

-spec clear(reference()) ->
    ok | {error, any()}.

clear(Ref) ->
    etrie_nif:clear(Ref).

-spec longest_prefix(reference(), binary()) ->
    null | {ok, MatchingKey::term(), MatchingValue::term()} | {error, any()}.

longest_prefix(Ref, Prefix) ->
    etrie_nif:longest_prefix(Ref, Prefix).
