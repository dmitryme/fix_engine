/* @file   fix_msg.h
   @author Dmitry S. Melnikov, dmitryme@gmail.com
   @date   Created on: 11/07/2012 17:54:30 AM
*/

#include "common.h"

#include <erl_nif.h>
#include <fix_error.h>
#include <fix_msg.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static ERL_NIF_TERM ok_atom;
ErlNifResourceType* messages_res;

static void free_message(ErlNifEnv* env, void* obj)
{
   fix_msg_free((FIXMsg*)obj);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
   messages_res = enif_open_resource_type( env, NULL, "messages_res",
      free_message, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
   ok_atom = enif_make_atom(env, "ok");
   return 0;
}

static ERL_NIF_TERM create(ErlNifEnv* env, int argc, ERL_NIF_TERM const argv[])
{
   return ok_atom;
}

static ErlNifFunc nif_funcs[] =
{
   {"create", 2, create}
};

ERL_NIF_INIT(fix_msg, nif_funcs, load, NULL, NULL, NULL)
