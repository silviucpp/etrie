etrie
================

[![Build Status](https://travis-ci.com/silviucpp/etrie.svg?branch=master)](https://travis-ci.com/github/silviucpp/etrie)
[![GitHub](https://img.shields.io/github/license/silviucpp/etrie)](https://github.com/silviucpp/etrie/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/etrie)](https://hex.pm/packages/etrie)

A fast and memory efficient HAT-trie based on [Tessil hat-trie][1] (NIF based). Details regarding the HAT-trie data structure can be found [here][2].

Implementation details
-----------

- The etrie data structure reference should be operated from the same process where was created in order to avoid concurrent operation taking place.
- Like any other data an etrie reference is garbage collected by Erlang VM when there is no longer any reference to it. The references can be stored and passed between processes on the same node and also sored in an ETS table.
- The library provides an efficient and compact way to store a map of strings by compressing the common prefixes. It also allows to search for keys that match a prefix. 

**Note**: *The default parameters of the structure are geared toward optimizing exact searches, if you do a lot of prefix searches you may want to reduce the **burst threshold** through the `*_burst_threshold` methods or while you create the structure.*

Quick start
-----------

You need to have at least Erlang OTP 19. This is because `enif_binary_to_term` and `enif_term_to_binary` are not available in previous versions.

Compile:

```sh
rebar3 compile
```

Simple usage:

```erlang
{ok, T} = etrie:new(),
ok = etrie:insert(T, <<"one">>, 1),
ok = etrie:insert(T, <<"two">>, 2),
ok = etrie:insert(T, <<"three">>, 3),
ok = etrie:insert(T, <<"four">>, 4),
{ok,[{<<"four">>,4}, {<<"two">>,2}, {<<"three">>,3}, {<<"one">>,1}]} = etrie:to_list(T),
{ok, 4} = etrie:lookup(T, <<"four">>),
{ok,[{<<"two">>,2},{<<"three">>,3}]} = etrie:lookup_range(T, <<"t">>),
{ok, <<"three">>, 3} = etrie:longest_prefix(T, <<"three tables">>).
```

API
-----------

- `new()` - Create a new HAT-Trie instance.
- `new(BurstThreshold)` - Create a new HAT-Trie instance with specified `burst_threshold`.
- `from_list(Items)` -  - Create a new HAT-Trie from a proplist with specified `burst_threshold`.
- `from_list(Items, BurstThreshold)` - Create a new HAT-Trie from a proplist. 
- `to_list(Ref)` - Return all items from HAT-Trie into a proplist.
- `insert(Ref, Key, Value)` - Add a new item to the HAT-Trie.
- `remove(Ref, Key)` - Remove an item with a specified key from the HAT-Trie.
- `remove_prefix(Ref, Key)` - Remove all items with a specified prefix from the HAT-Trie.
- `clear(Ref)` - Remove all items from the HAT-Trie.
- `is_empty(Ref)` - Indicates if the HAT-Trie is empty or not.
- `size(Ref)` - Get the number of items from the HAT-Trie.
- `longest_prefix(Ref, Prefix)` - Get the item that match the longest prefix from the HAT-Trie.
- `lookup(Ref, Key)` - Get the item that match a certain key.
- `lookup_range(Ref, Prefix)` - Get all items that matches a certain key.
- `is_member(Ref, Key)` - Indicates if a key is member of the HAT-Trie.
- `get_burst_threshold(Ref)` - Get the current burst threshold value. 
- `set_burst_threshold(Ref, BurstThreshold)` - Adjust the burst threshold value.

Tests
------------

In order to run the integrity tests run `rebar3 eunit` from project root.

[1]: https://github.com/Tessil/hat-trie
[2]: https://tessil.github.io/2017/06/22/hat-trie.html

