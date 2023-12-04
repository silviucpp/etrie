#ifndef C_SRC_ETRIE_NIF_H_
#define C_SRC_ETRIE_NIF_H_

#include "erl_nif.h"

namespace etrie {

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomNull;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomBadArg;
};

struct etrie_data
{
    ErlNifResourceType* resEtrieInstance;
};

extern atoms ATOMS;

}

#endif  // C_SRC_ETRIE_NIF_H_
