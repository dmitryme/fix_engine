#include <erl_nif.h>
#include <linux/limits.h> // for PATH_MAX const
#include <fix_parser.h>
#include <stdarg.h>
#include <stdio.h>

ERL_NIF_TERM ok_atom;
ERL_NIF_TERM error_atom;

ERL_NIF_TERM make_error(ErlNifEnv* env, char const* errorMsg, ...)
{
   va_list ap;
   va_start(ap, errorMsg);
   char text[1024];
   vsnprintf(text, sizeof(text), errorMsg, ap);
   return enif_make_tuple2(env, error_atom, enif_make_string(env, text, ERL_NIF_LATIN1));
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
      return make_error(env, "First paremeter is invalid. Should be string.");
   }
   FIXParserAttrs attrs = {};
   ERL_NIF_TERM head;
   ERL_NIF_TERM tail = argv[1];
   int cnt = 0;
   while(enif_get_list_cell(env, tail, &head, &tail))
   {
      int arity;
      ERL_NIF_TERM const* tuple = NULL;
      if (!enif_get_tuple(env, head, &arity, &tuple) || arity != 2)
      {
         return make_error(env, "Wrong param arity. Should be tuple with arity 2.");
      }
      char pname[64] = {};
      if (!enif_get_atom(env, tuple[0], pname, sizeof(pname), ERL_NIF_LATIN1))
      {
         return make_error(env, "Wrong param name type. Should be atom.");
      }
      int val = 0;
      if (!enif_get_int(env, tuple[1], &val))
      {
         return make_error(env, "Wrong param '%s' value. Should be integer.", pname);
      }
      if (!strcmp("page_size", pname)) { attrs.pageSize = val; }
      else if (!strcmp("max_page_size", pname)) { attrs.maxPageSize = val; }
      else if (!strcmp("num_pages", pname)) { attrs.numPages = val; }
      else if (!strcmp("num_pages", pname)) { attrs.numPages = val; }
      else if (!strcmp("max_pages", pname)) { attrs.maxPages = val; }
      else if (!strcmp("num_groups", pname)) { attrs.numGroups = val; }
      else if (!strcmp("max_groups", pname)) { attrs.maxGroups = val; }
      else
      {
         return make_error(env, "Unsupported parameter name '%s'.", pname);
      }
   }
   return ok_atom;
}

static ErlNifFunc nif_funcs[] =
{
   {"create_parser", 3, create_parser}
};

ERL_NIF_INIT(erlang_fix, nif_funcs, load, NULL, NULL, NULL)