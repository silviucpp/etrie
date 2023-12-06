
PROJECT_NIF_NAME=etrie_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
	LDFLAGS +=  -Wl,-U,_enif_alloc \
				-Wl,-U,_enif_free \
				-Wl,-U,_enif_alloc_resource \
				-Wl,-U,_enif_release_resource \
				-Wl,-U,_enif_release_binary \
				-Wl,-U,_enif_alloc_env \
				-Wl,-U,_enif_free_env \
				-Wl,-U,_enif_clear_env \
				-Wl,-U,_enif_get_double \
				-Wl,-U,_enif_get_int \
				-Wl,-U,_enif_get_long \
				-Wl,-U,_enif_get_ulong \
				-Wl,-U,_enif_get_resource \
				-Wl,-U,_enif_get_uint \
				-Wl,-U,_enif_get_list_cell \
				-Wl,-U,_enif_get_tuple \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_is_list \
				-Wl,-U,_enif_make_copy \
				-Wl,-U,_enif_make_long \
				-Wl,-U,_enif_make_ulong \
				-Wl,-U,_enif_make_double \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_resource \
				-Wl,-U,_enif_make_tuple \
				-Wl,-U,_enif_binary_to_term \
				-Wl,-U,_enif_term_to_binary \
				-Wl,-U,_enif_term_type \
				-Wl,-U,_enif_priv_data \
				-Wl,-U,_enif_open_resource_type \
				-Wl,-U,_enif_inspect_binary \
				-Wl,-U,_enif_make_badarg
endif

CXXFLAGS += -DNDEBUG -g -Wextra -Werror -Wno-deprecated-declarations -Wno-missing-field-initializers -fno-rtti -std=c++17 \
			-I $(BASEDIR)/_build/deps/hat-trie/include
LDFLAGS  += -lstdc++
