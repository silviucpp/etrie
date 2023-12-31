#include "nif_data_mapping.h"
#include "nif_utils.h"
#include "etrie_nif.h"
#include "etrie.h"

namespace etrie {

// EtrieScope

NifDataMapping::EtrieScope::EtrieScope(void* trie) : trie_(trie)
{
    enif_keep_resource(trie_);
}

NifDataMapping::EtrieScope::~EtrieScope()
{
    if(trie_)
        enif_release_resource(trie_);
}

NifDataMapping::EtrieScope::EtrieScope(EtrieScope&& other)
{
    *this = std::move(other);
}

NifDataMapping::EtrieScope& NifDataMapping::EtrieScope::operator=(NifDataMapping::EtrieScope&& other)
{
    if (this != &other)
    {
        trie_ = other.trie_;
        other.trie_ = nullptr;
   }
   return *this;
}

// NifDataMapping

NifDataMapping::NifDataMapping(ErlNifEnv* env, ERL_NIF_TERM term): type_(ErlangDataType::UNSPECIFIED)
{
    switch(enif_term_type(env, term))
    {
        case ERL_NIF_TERM_TYPE_ATOM:
            type_ = ErlangDataType::ATOM;
            value_ = reinterpret_cast<ErlNifUInt64>(term);
            break;

        case ERL_NIF_TERM_TYPE_BITSTRING:
        {
            ErlNifBinary bin;
            if(enif_inspect_binary(env, term, &bin))
            {
                value_ = std::string(BIN_TO_STR(bin.data), bin.size);
                type_  = ErlangDataType::BITSTRING;
            }
            break;
        }

        case ERL_NIF_TERM_TYPE_FLOAT:
        {
            double d;
            if(enif_get_double(env, term, &d))
            {
                value_ = d;
                type_ = ErlangDataType::DOUBLE;
            }
            break;
        }

        case ERL_NIF_TERM_TYPE_INTEGER:
        {
             ErlNifSInt64 i_v;
             ErlNifUInt64 ui_v;

             if(enif_get_int64(env, term, &i_v))
             {
                value_ = i_v;
                type_ = ErlangDataType::INT64;
             }
             else if(enif_get_uint64(env, term, &ui_v))
             {
                value_ = ui_v;
                type_ = ErlangDataType::UINT64;
             }
             break;
        }
        case ERL_NIF_TERM_TYPE_REFERENCE:
        {
            void* trie = get_abstract_trie_object(env, term);

            if(trie)
            {
                value_ = EtrieScope(trie);
                type_ = TRIE;
            }
        }

        default:
            break;
    };

    if(type_ == ErlangDataType::UNSPECIFIED)
    {
         ErlNifBinary bin;
         if(enif_term_to_binary(env, term, &bin))
         {
            value_ = bin;
            type_ = ErlangDataType::BIN_TERM;
         }
    }
}

NifDataMapping::~NifDataMapping()
{
    if(type_ == ErlangDataType::BIN_TERM)
    {
        enif_release_binary(&std::get<ErlNifBinary>(value_));
    }
}

NifDataMapping::NifDataMapping(NifDataMapping&& other) noexcept
{
    *this = std::move(other);
}

NifDataMapping& NifDataMapping::operator=(NifDataMapping&& other) noexcept
{
    if (this != &other)
    {
        type_ = other.type_;
        value_ = std::move(other.value_);
        other.type_ = ErlangDataType::UNSPECIFIED;
   }
   return *this;
}

bool NifDataMapping::to_nif(ErlNifEnv* env, ERL_NIF_TERM* term) const
{
    try
    {
        switch(type_)
        {
            case ATOM:
                *term = reinterpret_cast<ERL_NIF_TERM>(std::get<ErlNifUInt64>(value_));
                return true;

            case BITSTRING:
                *term = make_binary(env, std::get<std::string>(value_));
                return true;

            case DOUBLE:
                *term = enif_make_double(env, std::get<double>(value_));
                return true;

            case INT64:
                *term = enif_make_int64(env, std::get<ErlNifSInt64>(value_));
                return true;

            case UINT64:
                *term = enif_make_uint64(env, std::get<ErlNifUInt64>(value_));
                return true;

            case BIN_TERM:
            {
                ErlNifBinary bin = std::get<ErlNifBinary>(value_);
                return enif_binary_to_term(env, bin.data, bin.size, term, 0) != 0;
            }

            case TRIE:
            {
                *term = enif_make_resource(env,std::get<EtrieScope>(value_).ptr());
                return true;
            }

            default:
                return false;
        }
    }
    catch (const std::bad_variant_access& ex)
    {
        return false;
    }
}

}
