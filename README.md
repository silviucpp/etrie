# etrie

[![Build Status](https://travis-ci.com/silviucpp/etrie.svg?branch=master)](https://travis-ci.com/github/silviucpp/etrie)
[![GitHub](https://img.shields.io/github/license/silviucpp/etrie)](https://github.com/silviucpp/etrie/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/etrie)](https://hex.pm/packages/etrie)

**A Fast and Memory-Efficient HAT-Trie Implementation Based on [Tessil hat-trie][1] (NIF based). Learn more about the HAT-Trie data structure [here][2].**

## Implementation Details

- The `etrie` data structure reference should be operated from the same process where it was created to avoid concurrent operations.
- An `etrie` reference is garbage-collected by the Erlang VM when there is no longer any reference to it. References can be stored, passed between processes on the same node, and stored in an ETS table.
- Methods such as `new/1`, `from_list/1`, `from_list/2`, `to_list/1` are implemented as dirty NIFs to handle large lists efficiently and prevent Erlang scheduler blocking.

### Notes

- The library provides an efficient and compact way to store a map of strings by compressing common prefixes. It also allows searching for keys that match a prefix.
- The default parameters are optimized for exact searches. If you perform many prefix searches, consider adjusting the **burst threshold** `(default to 16384)` through the `*_burst_threshold` methods or during structure creation.
- Keys must be binary strings, and the maximum key size is limited to `65534` bytes.
- The capacity of the HAT-Trie is restricted to a maximum of `4294967296` items.

## Quick Start

Ensure you have at least Erlang OTP 19.0, as `enif_binary_to_term` and `enif_term_to_binary` are not available in previous versions.

**Compile:**

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

## API

- `new()` - Creates a new HAT-Trie instance.
- `new(BurstThreshold)` - Creates a new HAT-Trie instance with a specified `burst_threshold`.
- `from_list(Items)` - Creates a new HAT-Trie from a proplist.
- `from_list(Items, BurstThreshold)` - Creates a new HAT-Trie from a proplist with a specified `burst_threshold`.
- `to_list(Ref)` - Returns all items from the HAT-Trie as a proplist.
- `insert(Ref, Key, Value)` - Adds a new item to the HAT-Trie.
- `remove(Ref, Key)` - Removes an item with a specified key from the HAT-Trie.
- `remove_prefix(Ref, Key)` - Removes all items with a specified prefix from the HAT-Trie.
- `clear(Ref)` - Removes all items from the HAT-Trie.
- `is_empty(Ref)` - Indicates if the HAT-Trie is empty or not.
- `size(Ref)` - Gets the number of items in the HAT-Trie.
- `longest_prefix(Ref, Prefix)` - Gets the item that matches the longest prefix in the HAT-Trie.
- `lookup(Ref, Key)` - Gets the item that matches a certain key.
- `lookup_range(Ref, Prefix)` - Gets all items that match a certain key.
- `is_member(Ref, Key)` - Indicates if a key is a member of the HAT-Trie.
- `get_burst_threshold(Ref)` - Gets the current burst threshold value.
- `set_burst_threshold(Ref, BurstThreshold)` - Adjusts the burst threshold value.

## Tests

In order to run the integrity tests run `rebar3 eunit` from project root.

[1]: https://github.com/Tessil/hat-trie
[2]: https://tessil.github.io/2017/06/22/hat-trie.html

