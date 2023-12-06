-module(etrie_nif).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(load_nif/0).

-export([
    new/1,
    new/2,
    insert/3,
    remove/3,
    clear/1,
    is_empty/1,
    size/1,
    longest_prefix/2,
    lookup/3,
    is_member/2,
    to_list/1,
    get_burst_threshold/1,
    set_burst_threshold/2
]).

% nif functions

load_nif() ->
    SoName = get_priv_path(?MODULE),
    io:format(<<"Loading library: ~p ~n">>, [SoName]),
    ok = erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

new(_List) ->
    ?NOT_LOADED.

new(_List, _BurstThreshold) ->
    ?NOT_LOADED.

insert(_Ref, _Key, _Value) ->
    ?NOT_LOADED.

remove(_Ref, _Key, _Prefix) ->
    ?NOT_LOADED.

clear(_Ref) ->
    ?NOT_LOADED.

is_empty(_Ref) ->
    ?NOT_LOADED.

size(_Ref) ->
    ?NOT_LOADED.

longest_prefix(_Ref, _IsPrefix) ->
    ?NOT_LOADED.

lookup(_Ref, _Key, _IsPrefix) ->
    ?NOT_LOADED.

is_member(_Ref, _Prefix) ->
    ?NOT_LOADED.

to_list(_Ref) ->
    ?NOT_LOADED.

get_burst_threshold(_Ref) ->
    ?NOT_LOADED.

set_burst_threshold(_Ref, _BurstThreshold) ->
    ?NOT_LOADED.

% internals

get_priv_path(File) ->
    get_priv_path(etrie, ?MODULE, File).

get_priv_path(AppName, Module, File) ->
    case code:priv_dir(AppName) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Module)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.
