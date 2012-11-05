#include <erl_nif.h>
#include <linux/limits.h> // for PATH_MAX const

ERL_NIF_TERM ok_atom;
ERL_NIF_TERM error_atom;

ERL_NIF_TERM make_error(ErlNifEnv* env, char const* errorMsg)
{
   return enif_make_tuple2(env, error_atom, enif_make_string(env, errorMsg, ERL_NIF_LATIN1));
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  /*ErlNifResourceFlags flags;*/
  /*sick_handle_type = enif_open_resource_type(env, MODULE, "sick_handle_type",*/
      /*NULL, ERL_NIF_RT_CREATE, &flags);*/
  ok_atom = enif_make_atom(env, "ok");
  error_atom = enif_make_atom(env, "error");
  return 0;
}

static ERL_NIF_TERM create_parser(ErlNifEnv* env, int argc, ERL_NIF_TERM const argv[])
{
   char path[PATH_MAX] = {};
   int res = enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1);
   if (res <= 0)
   {
      return make_error(env, "First paremeter is invalid.");
   }
   return enif_make_string(env, path, ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
   {"create_parser", 3, create_parser}
};

ERL_NIF_INIT(erlang_fix, nif_funcs, load, NULL, NULL, NULL)
