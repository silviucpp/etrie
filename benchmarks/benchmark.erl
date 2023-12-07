-module(benchmark).

-export([
    run/3
]).

run(FileDataset, Module, Limit) ->
    io:format("## start loading items ... ~n", []),

    case read_file(FileDataset, Limit, fun(X) -> X end) of
        {ok, LinesCount, Lines} ->
            io:format("## loading complete. start benchmark for module: ~p ... ~n", [Module]),
            Ref0 = trie_new(Module),

            % test insert

            Ref1 = run_benchmark(Module, <<"insert">>, true, Ref0, Lines, LinesCount, fun(TrieRef, {Key, Value}) -> trie_insert(Module, TrieRef, Key, Value) end),
            Ref2 = run_benchmark(Module, <<"lookup">>, false, Ref1, Lines, LinesCount, fun(TrieRef, {Key, _}) -> trie_lookup(Module, TrieRef, Key) end),
            Ref3 = run_benchmark(Module, <<"remove">>, true, Ref2, Lines, LinesCount, fun(TrieRef, {Key, _}) -> trie_remove(Module, TrieRef, Key) end),
            0 = trie_size(Module, Ref3),
            ok;
        Error ->
            Error
    end.

% internals

run_benchmark(Module, Tag, UpdateRef, TrieRef0, Lines, LinesCount, Fun) ->
    {_MemStart, StartTs} = time_and_mem(),
    Result = case UpdateRef of
        true ->
            lists:foldl(fun(Line, TrieRef) -> Fun(TrieRef, Line) end, TrieRef0, Lines);
        _ ->
            lists:foreach(fun(Line) -> Fun(TrieRef0, Line) end, Lines),
            TrieRef0
    end,

    {_MemEnd, EndTs} = time_and_mem(),

    time_info(Module, Tag, StartTs, EndTs, LinesCount),
    %mem_info(Tag, MemStart, MemEnd),
    Result.

time_and_mem() ->
    {erlang:memory(), os:timestamp()}.

time_info(Module, Tag, StartTs, EndTs, OpsCount) ->
    TimeDiff = timer:now_diff(EndTs, StartTs),
    NsPerKey = (TimeDiff * 1000) / OpsCount,
    io:format("~p:~s Time -> operations: ~p elapsed: ~.4f ms, ~.4f ns/key~n", [Module, Tag, OpsCount, TimeDiff/1000, NsPerKey]),
    ok.

%%mem_info(_Tag, _MemStart0, _MemEnd0) ->
%%    MemStart = proplists:get_value(total, MemStart0),
%%    MemEnd = proplists:get_value(total, MemEnd0),
%%    Diff = MemEnd - MemStart,
%%    io:format("~p Memory -> ~.4f MiB (~p bytes).~n", [Tag, Diff / (1024 * 1024), Diff]).

trie_new(etrie) ->
    {ok, Ref} = etrie:new(),
    Ref;
trie_new(ok_btrie) ->
    btrie:new();
trie_new(ok_trie) ->
    trie:new().

trie_insert(etrie, Trie, Key, Value) ->
    etrie:insert(Trie, Key, Value),
    Trie;
trie_insert(ok_btrie, Trie, Key, Value) ->
    btrie:append(Key, Value, Trie);
trie_insert(ok_trie, Trie, Key, Value) ->
    trie:append(binary_to_list(Key), Value, Trie).

trie_lookup(etrie, Trie, Key) ->
    {ok, _} = etrie:lookup(Trie, Key);
trie_lookup(ok_btrie, Trie, Key) ->
    {ok, _} = btrie:find(Key, Trie);
trie_lookup(ok_trie, Trie, Key) ->
    {ok, _} = trie:find(binary_to_list(Key), Trie).

trie_remove(etrie, Trie, Key) ->
    etrie:remove(Trie, Key),
    Trie;
trie_remove(ok_btrie, Trie, Key) ->
    btrie:erase(Key, Trie);
trie_remove(ok_trie, Trie, Key) ->
    trie:erase(binary_to_list(Key), Trie).

trie_size(etrie, Trie) ->
    etrie:size(Trie);
trie_size(ok_btrie, Trie) ->
    btrie:size(Trie);
trie_size(ok_trie, Trie) ->
    trie:size(Trie).

read_file(FilePath, Limit, ValFun) ->
    case file:open(FilePath, [read, raw, binary]) of
        {ok, FileDescriptor} ->
            Response = read_lines(FileDescriptor, Limit, ValFun, 0, []),
            file:close(FileDescriptor),
            Response;
        {error, Reason} = R ->
            io:format("Error opening file: ~p~n", [Reason]),
            R
    end.

read_lines(FileDescriptor, Limit, ValFun, Index, AccumulatedLines) ->
    case file:read_line(FileDescriptor) of
        {ok, Line} ->
            case Limit < 1 orelse Index < Limit of
                true ->
                    read_lines(FileDescriptor, Limit, ValFun, Index+1, [{Line, ValFun(Index)} | AccumulatedLines]);
                _ ->
                    {ok, Index, AccumulatedLines}
            end;
        eof ->
            {ok, Index, AccumulatedLines};
        {error, Reason} = R ->
            io:format("Error reading line: ~p~n", [Reason]),
            R
    end.

