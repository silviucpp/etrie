#include "etrie_nif.h"
#include "nif_utils.h"
#include "macros.h"
#include "etrie.h"

#include <stdlib.h>

namespace etrie {

namespace {

const char kAtomOk[] = "ok";
const char kAtomNull[] = "null";
const char kAtomError[] = "error";
const char kAtomTrue[] = "true";
const char kAtomFalse[] = "false";
const char kAtomBadArg[] = "badarg";

static ErlNifFunc nif_funcs[] = {
    {"new", 1, nif_etrie_new, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"new", 2, nif_etrie_new, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"is_empty", 1, nif_etrie_is_empty},
    {"size", 1, nif_etrie_size},
    {"get_burst_threshold", 1, nif_etrie_get_burst_threshold},
    {"set_burst_threshold", 2, nif_etrie_set_burst_threshold},
    {"clear", 1, nif_etrie_clear},
    {"longest_prefix", 2, nif_etrie_longest_prefix},
    {"lookup", 2, nif_etrie_lookup},
    {"is_member", 2, nif_etrie_is_member},
    {"insert", 3, nif_etrie_insert},
    {"remove", 3, nif_etrie_remove},
};

void open_resources(ErlNifEnv* env, etrie_data* data)
{
    ErlNifResourceFlags flags =  static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    data->resEtrieInstance = enif_open_resource_type(env, NULL, "enif_etrie", nif_etrie_free, flags, NULL);
}

}

atoms ATOMS;

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomOk = make_atom(env, kAtomOk);
    ATOMS.atomNull = make_atom(env, kAtomNull);
    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomTrue = make_atom(env, kAtomTrue);
    ATOMS.atomFalse = make_atom(env, kAtomFalse);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);

    etrie_data* data = static_cast<etrie_data*>(enif_alloc(sizeof(etrie_data)));
    open_resources(env, data);
    *priv_data = data;
    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    etrie_data* data = static_cast<etrie_data*>(priv_data);
    enif_free(data);
}

int on_nif_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    UNUSED(old_priv);
    UNUSED(info);

    etrie_data* data = static_cast<etrie_data*>(enif_alloc(sizeof(etrie_data)));
    open_resources(env, data);
    *priv = data;
    return 0;
}

}

ERL_NIF_INIT(etrie_nif, etrie::nif_funcs, etrie::on_nif_load, NULL, etrie::on_nif_upgrade, etrie::on_nif_unload)
