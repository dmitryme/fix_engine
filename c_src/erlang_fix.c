#include <erl_nif.h>
#include <linux/limits.h> // for PATH_MAX const
#include <fix_parser.h>
#include <fix_error.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

ERL_NIF_TERM ok_atom;
ERL_NIF_TERM error_atom;
ErlNifResourceType* parsers_res;
ErlNifResourceType* messages_res;

static void free_parser(ErlNifEnv* env, void* obj)
{
   fix_parser_free((FIXParser*)obj);
}

static void free_message(ErlNifEnv* env, void* obj)
{
   fix_msg_free((FIXMsg*)obj);
}

static ERL_NIF_TERM make_error(ErlNifEnv* env, char const* errorMsg, ...)
{
   va_list ap;
   va_start(ap, errorMsg);
   char text[1024];
   vsnprintf(text, sizeof(text), errorMsg, ap);
   return enif_make_tuple2(env, error_atom, enif_make_string(env, text, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM make_parser_error(ErlNifEnv* env, int errCode, char const* errText)
{
   return enif_make_tuple2(
         env, error_atom, enif_make_tuple2(
            env, enif_make_int(env, errCode), enif_make_string(env, errText, ERL_NIF_LATIN1)));
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
   parsers_res = enif_open_resource_type( env, NULL, "parsers_res",
      free_parser, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

   messages_res = enif_open_resource_type( env, NULL, "messages_res",
      free_message, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

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
   tail = argv[2];
   int flags = 0;
   while(enif_get_list_cell(env, tail, &head, &tail))
   {
      char flag[64] = {};
      enif_get_atom(env, head, flag, sizeof(flag), ERL_NIF_LATIN1);
      if (!strcmp(flag, "check_crc")) { flags |= PARSER_FLAG_CHECK_CRC; }
      else if (!strcmp(flag, "check_required")) { flags |= PARSER_FLAG_CHECK_REQUIRED; }
      else if (!strcmp(flag, "check_value")) { flags |= PARSER_FLAG_CHECK_VALUE; }
      else if (!strcmp(flag, "check_unknown_fields")) { flags |= PARSER_FLAG_CHECK_UNKNOWN_FIELDS; }
      else if (!strcmp(flag, "check_all")) { flags |= PARSER_FLAG_CHECK_ALL; }
      else
      {
         return make_error(env, "Unsupported flag '%s'.", flag);
      }
   }
   FIXParser* parser = fix_parser_create(path, &attrs, flags);
   if (!parser)
   {
      return make_parser_error(env, get_fix_error_code(), get_fix_error_text());
   }
   FIXParser** pres = (FIXParser**)enif_alloc_resource(parsers_res, sizeof(FIXParser*));
   *pres = parser;
   ERL_NIF_TERM pres_term = enif_make_resource(env, pres);
   enif_release_resource(pres);
   return enif_make_tuple2(env, ok_atom, enif_make_tuple2(env, enif_make_ref(env), pres_term));
}

static ErlNifFunc nif_funcs[] =
{
   {"create_parser", 3, create_parser}
};

ERL_NIF_INIT(erlang_fix, nif_funcs, load, NULL, NULL, NULL)