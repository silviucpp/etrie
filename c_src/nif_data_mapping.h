#ifndef C_SRC_NIF_DATA_MAPPING_H_
#define C_SRC_NIF_DATA_MAPPING_H_

#include "erl_nif.h"
#include "macros.h"
#include <string>
#include <variant>

namespace etrie {

class NifDataMapping {

public:

    NifDataMapping(ErlNifEnv* env, ERL_NIF_TERM term);
    ~NifDataMapping();

    NifDataMapping(NifDataMapping&& other) noexcept;
    NifDataMapping& operator=(NifDataMapping&& other) noexcept;

    bool is_valid() const { return type_ != ErlangDataType::UNSPECIFIED;}
    bool to_nif(ErlNifEnv* env, ERL_NIF_TERM* term) const;

private:

    DISALLOW_IMPLICIT_CONSTRUCTORS(NifDataMapping);

    enum ErlangDataType {
       UNSPECIFIED,
       ATOM,
       BITSTRING,
       DOUBLE,
       INT64,
       UINT64,
       BIN_TERM
   };

    ErlangDataType type_;
    std::variant<std::string, double, ErlNifSInt64, ErlNifUInt64, ErlNifBinary> value_;
};

}

#endif  // C_SRC_NIF_DATA_MAPPING_H_
