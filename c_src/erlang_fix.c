#include <erl_nif.h>

static ERL_NIF_TERM create_parser(ErlNifEnv* env, int argc, ERL_NIF_TERM const argv[])
{
   return enif_make_string(env, "Hello!", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
   {"create_parser", 3, create_parser}
};

ERL_NIF_INIT(erlang_fix, nif_funcs, NULL, NULL, NULL, NULL)
