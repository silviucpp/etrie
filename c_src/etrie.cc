#include "etrie.h"
#include "etrie_nif.h"
#include "nif_utils.h"
#include "macros.h"
#include "nif_data_mapping.h"

#include <tsl/htrie_map.h>
#include <memory>

namespace etrie {

namespace {

const size_t DEFAULT_BURST_THRESHOLD = 16384;
typedef tsl::htrie_map<char, NifDataMapping> TrieHashMap;

struct enif_etrie
{
    TrieHashMap* trie;
};

inline enif_etrie* nif_etrie_new(etrie_data* data)
{
    return static_cast<enif_etrie*>(enif_alloc_resource(data->resEtrieInstance, sizeof(enif_etrie)));
}

inline enif_etrie* get_enif_trie(ErlNifEnv* env, ERL_NIF_TERM term)
{
    etrie_data* data = static_cast<etrie_data*>(enif_priv_data(env));
    enif_etrie* enif_obj;

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
        delete data->trie;
}

ERL_NIF_TERM nif_etrie_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM list = argv[0];

    if(!enif_is_list(env, list))
        return make_badarg(env);

    etrie_data* data = static_cast<etrie_data*>(enif_priv_data(env));

    scoped_ptr(enif_obj, enif_etrie, nif_etrie_new(data), enif_release_resource);

    if(enif_obj.get() == NULL)
        return make_error(env, "enif_alloc_resource failed");

    ErlNifUInt64 burst_threshold = DEFAULT_BURST_THRESHOLD;

    if(argc > 1 && !enif_get_uint64(env, argv[1], &burst_threshold))
        return make_badarg(env);

    enif_obj->trie = new TrieHashMap(burst_threshold);

    ERL_NIF_TERM head;
    const ERL_NIF_TERM *items;
    int arity;

    ErlNifBinary key;

    while(enif_get_list_cell(env, list, &head, &list))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return make_badarg(env);

        if(!get_binary(env, items[0], &key))
            return make_badarg(env);

        NifDataMapping value(env, items[1]);

        if(!value.is_valid())
            return make_badarg(env);

        try
        {
            enif_obj->trie->insert_ks(BIN_TO_STR(key.data), key.size, std::move(value));
        }
        catch (std::exception &e)
        {
            return make_error(env, e.what());
        }
    }

    return make_ok_result(env, enif_make_resource(env, enif_obj.get()));
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

    enif_obj->trie->clear();
    return ATOMS.atomOk;
}

ERL_NIF_TERM nif_etrie_longest_prefix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    ErlNifBinary prefix;

    if(!get_binary(env, argv[1], &prefix))
        return make_badarg(env);

    // Find longest match prefix.
    auto it = enif_obj->trie->longest_prefix_ks(BIN_TO_STR(prefix.data), prefix.size);

    if(it == enif_obj->trie->end())
        return ATOMS.atomNull;

    ERL_NIF_TERM value_term;
    if(!it.value().to_nif(env, &value_term))
        return make_error(env, "failed to decode data");

    return enif_make_tuple(env, 3, ATOMS.atomOk, make_binary(env, it.key()), value_term);
}

ERL_NIF_TERM nif_etrie_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    ErlNifBinary key;
    bool prefix;

    if(!get_binary(env, argv[1], &key) || !get_boolean(argv[2], &prefix))
        return make_badarg(env);

    if(prefix)
    {
        auto prefix_range = enif_obj->trie->equal_prefix_range_ks(BIN_TO_STR(key.data), key.size);
        std::string key_buffer;
        ERL_NIF_TERM list = enif_make_list(env, 0);

        for(auto it = prefix_range.first; it != prefix_range.second; ++it)
        {
            it.key(key_buffer);
            ERL_NIF_TERM value_term;

            if(!it.value().to_nif(env, &value_term))
                return make_error(env, "failed to decode data");

            list = enif_make_list_cell(env, enif_make_tuple2(env, make_binary(env, key_buffer), value_term), list);
        }

        return make_ok_result(env, list);
    }
    else
    {
        try
        {
            auto value = &enif_obj->trie->at_ks(BIN_TO_STR(key.data), key.size);

            ERL_NIF_TERM value_term;
            if(!value->to_nif(env, &value_term))
                return make_error(env, "failed to decode data");

            return make_ok_result(env, value_term);
        }
        catch (std::out_of_range &ex)
        {
            return ATOMS.atomNull;
        }
    }

}

ERL_NIF_TERM nif_etrie_is_member(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    ErlNifBinary key;

    if(!get_binary(env, argv[1], &key))
        return make_badarg(env);

    return make_ok_result(env, enif_obj->trie->count_ks(BIN_TO_STR(key.data), key.size)? ATOMS.atomTrue : ATOMS.atomFalse);
}

ERL_NIF_TERM nif_etrie_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    std::string key_buffer;
    ERL_NIF_TERM list = enif_make_list(env, 0);

    for(auto it = enif_obj->trie->begin(); it != enif_obj->trie->end(); ++it)
    {
        it.key(key_buffer);
        ERL_NIF_TERM value_term;

        if(!it.value().to_nif(env, &value_term))
            return make_error(env, "failed to decode data");

        list = enif_make_list_cell(env, enif_make_tuple2(env, make_binary(env, key_buffer), value_term),list);
    }

    return make_ok_result(env, list);
}

ERL_NIF_TERM nif_etrie_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    ErlNifBinary key;
    NifDataMapping value(env, argv[2]);

    if(!get_binary(env, argv[1], &key) || !value.is_valid())
        return make_badarg(env);

    try
    {
        enif_obj->trie->insert_ks(BIN_TO_STR(key.data), key.size, std::move(value));
    }
    catch (std::exception &e)
    {
        return make_error(env, e.what());
    }

    return ATOMS.atomOk;
}

ERL_NIF_TERM nif_etrie_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    enif_etrie* enif_obj = get_enif_trie(env, argv[0]);

    if(enif_obj == nullptr)
        return make_badarg(env);

    ErlNifBinary key;
    bool prefix;

    if(!get_binary(env, argv[1], &key) || !get_boolean(argv[2], &prefix))
        return make_badarg(env);

    size_t elements;

    if(prefix)
        elements = enif_obj->trie->erase_prefix_ks(BIN_TO_STR(key.data), key.size);
    else
        elements = enif_obj->trie->erase_ks(BIN_TO_STR(key.data), key.size);

    return make_ok_result(env, enif_make_uint64(env, elements));
}

}
