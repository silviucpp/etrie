#ifndef C_SRC_ETRIE_H_
#define C_SRC_ETRIE_H_

#include "erl_nif.h"

namespace etrie {

void nif_etrie_free(ErlNifEnv* env, void* obj);
ERL_NIF_TERM nif_etrie_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_get_burst_threshold(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_set_burst_threshold(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_longest_prefix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_is_member(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_etrie_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

}

#endif  // C_SRC_ETRIE_H_
