

#include <erl_nif.h>
#include <fix_error.h>
#include <stdarg.h>
#include <stdio.h>

static ERL_NIF_TERM make_error(ErlNifEnv* env, char const* errorMsg, ...)
{
   va_list ap;
   va_start(ap, errorMsg);
   char text[1024];
   vsnprintf(text, sizeof(text), errorMsg, ap);
   return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, text, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM make_parser_error(ErlNifEnv* env, int errCode, char const* errText)
{
   return enif_make_tuple2(
         env, enif_make_atom(env, "error"), enif_make_tuple2(
            env, enif_make_int(env, errCode), enif_make_string(env, errText, ERL_NIF_LATIN1)));
}