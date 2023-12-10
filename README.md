# etrie

[![Build Status](https://travis-ci.com/silviucpp/etrie.svg?branch=main)](https://travis-ci.com/github/silviucpp/etrie)
[![GitHub](https://img.shields.io/github/license/silviucpp/etrie)](https://github.com/silviucpp/etrie/blob/main/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/etrie)](https://hex.pm/packages/etrie)

A Fast and Memory-Efficient HAT-Trie Implementation Based on [Tessil hat-trie][1] (NIF based). Learn more about the HAT-Trie data structure [here][2].

## Implementation Details

- The `etrie` data structure reference should be operated from the same process where it was created to avoid concurrent operations.
- An `etrie` reference is garbage-collected by the Erlang VM when there is no longer any reference to it. References can be stored, passed between processes on the same node, and stored in an ETS table.
- Methods such as `new/1`, `from_list/1`, `from_list/2`, `to_list/1` are implemented as dirty NIFs to handle large lists efficiently and prevent Erlang scheduler blocking.
- `etrie` stores the HAT-Trie values of types such as atoms, numeric, or binary strings as native data (`uint64`, `int64`, or `double` for numeric data, `uint64` for atoms and `std::string` for binary strings). The other data types like lists, tuples, etc., the storage is done by encoding the value using `erlang:term_to_binary`, which may contribute to additional CPU usage and memory consumption. It's essential to note that beside the data type itself there is some other additional overhead introduced by the wrappers and the use of `std::variant` (a C++ type-safe `union`) but should be insignificant.

### Notes

- The library provides an efficient and compact way to store a map of strings by compressing common prefixes. It also allows searching for keys that match a prefix.
- The default parameters are optimized for exact searches. If you perform many prefix searches, consider adjusting the **burst threshold** `(default to 16384)` through the `*_burst_threshold` methods or during structure creation.
- Keys must be binary strings, and the maximum key size is limited to `65534` bytes.
- The capacity of the HAT-Trie is restricted to a maximum of `4294967296` items.
- Due to the limitations of the NIF API, references returned from other NIFs are encoded using `term_to_binary/1`. If you do not store a reference to these externally, there is a possibility that they may be subject to garbage collection. It is important to note that references to `etrie` objects are appropriately managed.

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

## Benchmark

The benchmarking process is facilitated by the Erlang module (`benchmarks/benchmark.erl`), which reads a designated dataset line by line. Each line in the dataset represents a key in the trie, and its associated value is the line number.
The dataset is utilized to perform various operations such as insertions, reads, and deletions, with the elapsed time measured for each operation.

The dataset used represents all the titles from the main namespace of the [Wikipedia archive][3].

To run the benchmark:

```erlang
make benchmark MODULE=etrie DATASET=/Downloads/enwiki-latest-all-titles-in-ns0 LIMIT=1000000
```

Where:

- `MODULE`: the library used to benchmark. Can be one of : `etrie`, `ok_btrie` or `ok_trie`.
- `DATASET`: file path of the dataset.
- `LIMIT`: Maximum number of items to load from dataset. If < 1 then load the entire dataset.

#### Results

Unfortunately the [okeuday trie][4] implementation (`ok_btrie`  and `ok_trie`) it's pretty slow and I had to limit the number of items to 2 millions. 

Results are:

```
etrie:insert Time -> operations: 2000000 elapsed: 881.1570 ms, 440.5785 ns/key
etrie:lookup Time -> operations: 2000000 elapsed: 454.7810 ms, 227.3905 ns/key
etrie:remove Time -> operations: 2000000 elapsed: 532.2440 ms, 266.1220 ns/key

ok_btrie:insert Time -> operations: 2000000 elapsed: 60761.0590 ms, 30380.5295 ns/key
ok_btrie:lookup Time -> operations: 2000000 elapsed: 5925.6150 ms, 2962.8075 ns/key
ok_btrie:remove Time -> operations: 2000000 elapsed: 38510.2600 ms, 19255.1300 ns/key

ok_trie:insert Time -> operations: 2000000 elapsed: 61013.2730 ms, 30506.6365 ns/key
ok_trie:lookup Time -> operations: 2000000 elapsed: 2283.5370 ms, 1141.7685 ns/key
ok_trie:remove Time -> operations: 2000000 elapsed: 35662.8770 ms, 17831.4385 ns/key

```

#### Memory usage

I faced difficulty in finding an appropriate method to programmatically measure the memory consumption specifically utilized by the trie itself across different libraries. The challenge stems from the fact that the memory allocation by the `etrie` NIFs is not exclusively performed with `enif_*` methods. Consequently, Erlang lacks detailed statistics regarding the memory allocated directly through native C++ allocation methods.
To gain an understanding of memory allocation, I manually observed the memory usage of the `beam.smp` process using the `Activity Monitor` on OSX. 

Of course the memory reported contains many other things beside the one used by the trie itself but because of the big gap it's clear that `etrie` uses less memory.   

Results are:

```sh
etrie: 680 MB
ok_btrie: 4.67 GB
ok_trie: 5.42 GB
```

**Note:** The test employs the line index as a value, which is an `integer` and stored by `etrie` as a native data type. For other values lie tuples, lists, (see `Implementation Details` section) there might be additional overhead caused by `term_to_binary` encoding. 

## Tests

In order to run the integrity tests run `rebar3 eunit` from project root.

[1]: https://github.com/Tessil/hat-trie
[2]: https://tessil.github.io/2017/06/22/hat-trie.html
[3]: https://dumps.wikimedia.org/enwiki/latest/enwiki-latest-all-titles-in-ns0.gz
[4]: https://github.com/okeuday/trie
