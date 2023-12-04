#include "etrie.h"
#include "etrie_nif.h"
#include "nif_utils.h"
#include "macros.h"

#include <tsl/htrie_map.h>
#include <memory>

namespace etrie {

namespace {

const size_t DEFAULT_BURST_THRESHOLD = 16384;
const char kFailedToAllocResourceMsg[] = "enif_alloc_resource failed";
typedef tsl::htrie_map<char, ErlNifBinary> TrieHashMap;

struct enif_etrie
{
    TrieHashMap* trie;
};

enif_etrie* nif_etrie_new(etrie_data* data)
{
    return static_cast<enif_etrie*>(enif_alloc_resource(data->resEtrieInstance, sizeof(enif_etrie)));
}

enif_etrie* get_enif_trie(ErlNifEnv* env, ERL_NIF_TERM term)
{
    enif_etrie* enif_obj = nullptr;
    etrie_data* data = static_cast<etrie_data*>(enif_priv_data(env));

    if(enif_get_resource(env, term, data->resEtrieInstance, reinterpret_cast<void**>(&enif_obj)))
        return enif_obj;

    return nullptr;
}

}

void nif_etrie_free(ErlNifEnv* env, void* obj)
{
    UNUSED(env);

    enif_etrie* data = static_cast<enif_etrie*>(obj);

    if(data->trie)
    {
        for(auto it = data->trie->begin(); it != data->trie->end(); ++it)
            enif_release_binary(&it.value());

        delete data->trie;
    }
}

ERL_NIF_TERM nif_etrie_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM list = argv[0];

    if(!enif_is_list(env, list))
        return make_badarg(env);

    etrie_data* data = static_cast<etrie_data*>(enif_priv_data(env));

    scoped_ptr(enif_obj, enif_etrie, nif_etrie_new(data), enif_release_resource);
    unsigned long burst_threshold = DEFAULT_BURST_THRESHOLD;

    if(argc > 1 && !enif_get_uint64(env, argv[1], &burst_threshold))
        return make_badarg(env);

    if(enif_obj.get() == NULL)
        return make_error(env, etrie::kFailedToAllocResourceMsg);

    enif_obj->trie = new TrieHashMap(burst_threshold);

    ERL_NIF_TERM head;
    const ERL_NIF_TERM *items;
    int arity;

    ErlNifBinary key;
    ErlNifBinary value;

    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return make_badarg(env);

        if(!get_binary(env, items[0], &key))
            return make_badarg(env);

        if(!enif_term_to_binary(env, items[1], &value))
            return make_badarg(env);
        try
        {
            enif_obj->trie->insert(std::string_view(BIN_TO_STR(key.data), key.size), value);
        }
        catch (std::exception &e)
        {
            enif_release_binary(&value);
            return make_error(env, e.what());
        }
    }

    ERL_NIF_TERM term = enif_make_resource(env, enif_obj.get());
    return make_ok_result(env, term);
}

ERL_NIF_TERM nif_etrie_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return enif_make_badarg(env);

    return enif_obj->trie->empty() ? ATOMS.atomTrue: ATOMS.atomFalse;
}

ERL_NIF_TERM nif_etrie_get_burst_threshold(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return enif_make_badarg(env);

    return enif_make_uint64(env, enif_obj->trie->burst_threshold());
}

ERL_NIF_TERM nif_etrie_set_burst_threshold(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    unsigned long burst_threshold;

    if(enif_obj == nullptr ||!enif_get_uint64(env, argv[1], &burst_threshold))
        return make_badarg(env);

    enif_obj->trie->burst_threshold(burst_threshold);

    return ATOMS.atomOk;
}

ERL_NIF_TERM nif_etrie_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return enif_make_badarg(env);

    return enif_make_uint64(env, enif_obj->trie->size());
}

ERL_NIF_TERM nif_etrie_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    for(auto it = enif_obj->trie->begin(); it != enif_obj->trie->end(); ++it)
        enif_release_binary(&it.value());

    enif_obj->trie->clear();
    return ATOMS.atomOk;
}

ERL_NIF_TERM nif_etrie_longest_prefix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    ErlNifBinary bin;

    if(!get_binary(env, argv[1], &bin))
        return make_badarg(env);

    // Find longest match prefix.
    auto longest_prefix = enif_obj->trie->longest_prefix(std::string_view(BIN_TO_STR(bin.data), bin.size));

    if(longest_prefix == enif_obj->trie->end())
        return ATOMS.atomNull;

    ERL_NIF_TERM bin_term;

    if(enif_binary_to_term(env, longest_prefix.value().data, longest_prefix.value().size, &bin_term, 0) == 0)
        return make_error(env, "failed to decode data");

    return enif_make_tuple(env, 3, ATOMS.atomOk, make_binary(env, longest_prefix.key().c_str(), longest_prefix.key().size()),  bin_term);
}

ERL_NIF_TERM nif_etrie_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    ErlNifBinary key;
    ErlNifBinary value;

    if(!get_binary(env, argv[1], &key))
        return make_badarg(env);

    if(!enif_term_to_binary(env, argv[2], &value))
        return make_badarg(env);

    try
    {
        enif_obj->trie->insert(std::string_view(BIN_TO_STR(key.data), key.size), value);
    }
    catch (std::exception &e)
    {
        enif_release_binary(&value);
        return make_error(env, e.what());
    }

    return ATOMS.atomOk;
}

}
