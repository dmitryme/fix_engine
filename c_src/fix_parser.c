/**
 * @file   fix_parser.c
 * @author Dmitry S. Melnikov, dmitryme@gmail.com
 * @date   Created on: 11/07/2012 17:54:30 AM
 */

#include <erl_nif.h>
#include <linux/limits.h> // for PATH_MAX const
#include <fix_parser.h>
#include <fix_error.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>

#define DEF_BINARY_SIZE 1024

static ERL_NIF_TERM ok_atom;
static ERL_NIF_TERM error_atom;
static ERL_NIF_TERM parser_atom;
static ERL_NIF_TERM msg_atom;
static ERL_NIF_TERM group_atom;
static ErlNifResourceType* parser_res;
static ErlNifResourceType* message_res;
static ErlNifResourceType* group_res;

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM make_error(ErlNifEnv* env, char const* errorMsg, ...)
{
   va_list ap;
   va_start(ap, errorMsg);
   char text[1024];
   vsnprintf(text, sizeof(text), errorMsg, ap);
   return enif_make_tuple2(env, error_atom, enif_make_string(env, text, ERL_NIF_LATIN1));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM make_parser_error(ErlNifEnv* env, int32_t errCode, char const* errText)
{
   return enif_make_tuple2(
         env, error_atom, enif_make_tuple2(
            env, enif_make_int(env, errCode), enif_make_string(env, errText, ERL_NIF_LATIN1)));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static void free_fix_parser(ErlNifEnv* env, void* obj)
{
   fix_parser_free(*(FIXParser**)obj);
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static void free_fix_msg(ErlNifEnv* env, void* obj)
{
   fix_msg_free(*(FIXMsg**)obj);
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static int32_t load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
   parser_res = enif_open_resource_type( env, NULL, "erlang_fix_parser_res",
      free_fix_parser, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
   message_res = enif_open_resource_type( env, NULL, "erlang_fix_message_res",
      free_fix_msg, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
   group_res = enif_open_resource_type( env, NULL, "erlang_fix_group_res",
      NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
   ok_atom = enif_make_atom(env, "ok");
   error_atom = enif_make_atom(env, "error");
   parser_atom = enif_make_atom(env, "parser");
   msg_atom = enif_make_atom(env, "msg");
   group_atom = enif_make_atom(env, "group");
   return 0;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_parser(ErlNifEnv* env, ERL_NIF_TERM ref, ERL_NIF_TERM* pres, FIXParser** parser)
{
   ERL_NIF_TERM const* tuple = NULL;
   int32_t arity;
   if (!enif_get_tuple(env, ref, &arity, &tuple) || arity != 2)
   {
      return make_error(env, "Wrong parser reference.");
   }
   if (enif_compare(tuple[0], parser_atom))
   {
      return make_error(env, "Wrong parser reference.");
   }
   *pres = tuple[1];
   void* res = NULL;
   if (!enif_get_resource(env, *pres, parser_res, &res))
   {
      return make_error(env, "Wrong parser resource.");
   }
   *parser = *(FIXParser**)res;
   return ok_atom;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_parser_msg(
      ErlNifEnv* env, ERL_NIF_TERM ref,
      ERL_NIF_TERM* pres, ERL_NIF_TERM* mres,
      FIXParser** parser, FIXMsg** msg)
{
   int32_t arity;
   ERL_NIF_TERM const* tuple = NULL;
   if (!enif_get_tuple(env, ref, &arity, &tuple) || arity != 3)
   {
      return make_error(env, "Wrong msg reference.");
   }
   if (enif_compare(tuple[0], msg_atom))
   {
      return make_error(env, "Wrong msg reference.");
   }
   ERL_NIF_TERM const* data = NULL;
   if (!enif_get_tuple(env, tuple[2], &arity, &data) || arity != 2)
   {
      return make_error(env, "Wrong msg reference.");
   }
   *pres = data[0];
   *mres = data[1];
   void* res = NULL;
   if (!enif_get_resource(env, *pres, parser_res, &res))
   {
      return make_error(env, "Wrong parser resource.");
   }
   *parser = *(FIXParser**)res;

   if (!enif_get_resource(env, *mres, message_res, &res))
   {
      return make_error(env, "Wrong message resource.");
   }
   *msg = *(FIXMsg**)res;
   return ok_atom;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_parser_msg_group(
      ErlNifEnv* env, ERL_NIF_TERM ref,
      ERL_NIF_TERM* pres, ERL_NIF_TERM* mres, ERL_NIF_TERM* gres,
      FIXParser** parser, FIXMsg** msg, FIXGroup** group)
{
   int32_t arity;
   ERL_NIF_TERM const* tuple = NULL;
   if (!enif_get_tuple(env, ref, &arity, &tuple) || (arity != 2 && arity != 3))
   {
      return make_error(env, "Wrong msg reference1.");
   }
   if (enif_compare(tuple[0], msg_atom) && enif_compare(tuple[0], group_atom))
   {
      return make_error(env, "Wrong msg reference2.");
   }
   ERL_NIF_TERM const* data = NULL;
   if (arity == 3) // this is a message
   {
      if (!enif_get_tuple(env, tuple[2], &arity, &data) || arity != 2)
      {
         return make_error(env, "Wrong msg reference3.");
      }
   }
   else // this is a group
   {
      if (!enif_get_tuple(env, tuple[1], &arity, &data) || arity != 3)
      {
         return make_error(env, "Wrong msg reference4.");
      }
   }
   *pres = data[0];
   *mres = data[1];
   if (arity == 3) // group exists
   {
      *gres = data[2];
   }
   void* res = NULL;
   if (!enif_get_resource(env, *pres, parser_res, &res))
   {
      return make_error(env, "Wrong parser resource.");
   }
   *parser = *(FIXParser**)res;

   if (!enif_get_resource(env, *mres, message_res, &res))
   {
      return make_error(env, "Wrong message resource.");
   }
   *msg = *(FIXMsg**)res;
   if (arity == 3) // group exists
   {
      if (!enif_get_resource(env, *gres, group_res, &res))
      {
         return make_error(env, "Wrong group resource.");
      }
      *group = *(FIXGroup**)res;
   }
   else
   {
      *group = NULL;
   }
   return ok_atom;
}


/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM create(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   char path[PATH_MAX] = {};
   int32_t res = enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1);
   if (res <= 0)
   {
      return make_error(env, "First paremeter is invalid. Should be string.");
   }
   FIXParserAttrs attrs = {};
   ERL_NIF_TERM head;
   ERL_NIF_TERM tail = argv[1];
   while(enif_get_list_cell(env, tail, &head, &tail))
   {
      int32_t arity;
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
      int32_t val = 0;
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
   int32_t flags = 0;
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
      return make_parser_error(env, fix_error_get_code(), fix_error_get_text());
   }
   FIXParser** pres = (FIXParser**)enif_alloc_resource(parser_res, sizeof(FIXParser*));
   *pres = parser;
   ERL_NIF_TERM pres_term = enif_make_resource(env, pres);
   enif_release_resource(pres);
   return enif_make_tuple2(env, ok_atom, enif_make_tuple2(env, parser_atom, pres_term));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM create_msg(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM pres;
   FIXParser* parser = NULL;
   ERL_NIF_TERM res = get_parser(env, argv[0], &pres, &parser);
   if (res != ok_atom)
   {
      return res;
   }
   char msgType[12] = {};
   if (enif_get_string(env, argv[1], msgType, sizeof(msgType), ERL_NIF_LATIN1) <= 0)
   {
      return make_error(env, "Wrong msgType.");
   }
   FIXMsg* msg = fix_msg_create(parser, msgType);
   if (!msg)
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   FIXMsg** pmsg = (FIXMsg**)enif_alloc_resource(message_res, sizeof(FIXMsg*));
   *pmsg = msg;
   ERL_NIF_TERM pmsg_term = enif_make_resource(env, pmsg);
   enif_release_resource(pmsg);
   return enif_make_tuple2(
         env,
         ok_atom,
         enif_make_tuple3(env,
            msg_atom,
            enif_make_string(env, msgType, ERL_NIF_LATIN1),
            enif_make_tuple2(env, pres, pmsg_term)));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM add_group(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM pres;
   ERL_NIF_TERM mres;
   ERL_NIF_TERM gres;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &pres, &mres, &gres, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   FIXGroup* new_group = fix_msg_add_group(msg, group, tagNum);
   if (!new_group)
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   FIXGroup** grp = (FIXGroup**)enif_alloc_resource(group_res, sizeof(FIXGroup*));
   *grp = new_group;
   ERL_NIF_TERM grp_term = enif_make_resource(env, grp);
   enif_release_resource(grp);
   return enif_make_tuple2(env, ok_atom, enif_make_tuple2(
            env,
            group_atom,
            enif_make_tuple3(env, pres, mres, grp_term)));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_group(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM pres;
   ERL_NIF_TERM mres;
   ERL_NIF_TERM gres;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &pres, &mres, &gres, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   int32_t idx = 0;
   if (!enif_get_int(env, argv[2], &idx))
   {
      return make_error(env, "Wrong idx.");
   }
   FIXGroup* ret_group = fix_msg_get_group(msg, group, tagNum, idx);
   if (!ret_group)
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   FIXGroup** grp = (FIXGroup**)enif_alloc_resource(group_res, sizeof(FIXGroup*));
   *grp = ret_group;
   ERL_NIF_TERM grp_term = enif_make_resource(env, grp);
   enif_release_resource(grp);
   return enif_make_tuple2(env, ok_atom, enif_make_tuple2(
            env,
            group_atom,
            enif_make_tuple3(env, pres, mres, grp_term)));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM del_group(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   int32_t idx = 0;
   if (!enif_get_int(env, argv[2], &idx))
   {
      return make_error(env, "Wrong idx.");
   }
   if (FIX_FAILED == fix_msg_del_group(msg, group, tagNum, idx))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return ok_atom;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM set_int32_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   if (!enif_is_number(env, argv[2]))
   {
      return make_error(env, "Value is not a integer.");
   }
   int32_t val = 0;
   if (!enif_get_int(env, argv[2], &val))
   {
      return make_error(env, "Value not a 32-bit integer.");
   }
   if (FIX_FAILED == fix_msg_set_int32(msg, group, tagNum, val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return ok_atom;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM set_int64_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   if (!enif_is_number(env, argv[2]))
   {
      return make_error(env, "Value is not a integer.");
   }
   int64_t val = 0;
   if (!enif_get_int64(env, argv[2], &val))
   {
      return make_error(env, "Value not a 64-bit integer.");
   }
   if (FIX_FAILED == fix_msg_set_int64(msg, group, tagNum, val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return ok_atom;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM set_double_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   if (!enif_is_number(env, argv[2]))
   {
      return make_error(env, "Value is not a double.");
   }
   double val = 0.0;
   if (!enif_get_double(env, argv[2], &val))
   {
      enif_get_int64(env, argv[2], (int64_t*)&val);
   }
   if (FIX_FAILED == fix_msg_set_double(msg, group, tagNum, val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return ok_atom;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM set_string_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   if (!enif_is_list(env, argv[2]))
   {
      return make_error(env, "Value is not a string.");
   }
   uint32_t len = 0;
   enif_get_list_length(env, argv[2], &len);
   char* buff = malloc(len + 1);
   ERL_NIF_TERM ret = ok_atom;
   if (enif_get_string(env, argv[2], buff, len + 1, ERL_NIF_LATIN1) <= 0)
   {
      ret = make_error(env, "Unable to get string value.");
   }
   else
   {
      buff[len] = 0;
      if (FIX_FAILED == fix_msg_set_string(msg, group, tagNum, buff))
      {
         ret = make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
      }
   }
   free(buff);
   return ret;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM set_char_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   if (!enif_is_number(env, argv[2]))
   {
      return make_error(env, "Value is not a char.");
   }
   int32_t val = 0;
   enif_get_int(env, argv[2], &val);
   if (val < 32 || val > 126)
   {
      return make_error(env, "Value is not a char.");
   }
   if (FIX_FAILED == fix_msg_set_char(msg, group, tagNum, (char)val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return ok_atom;
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM set_data_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   return make_error(env, "not impemented yet");
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_int32_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   int32_t val = 0;
   if (FIX_FAILED == fix_msg_get_int32(msg, group, tagNum, &val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return enif_make_tuple2(env, ok_atom, enif_make_int(env, val));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_int64_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   int64_t val = 0;
   if (FIX_FAILED == fix_msg_get_int64(msg, group, tagNum, &val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return enif_make_tuple2(env, ok_atom, enif_make_int64(env, val));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_double_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   double val = 0;
   if (FIX_FAILED == fix_msg_get_double(msg, group, tagNum, &val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return enif_make_tuple2(env, ok_atom, enif_make_double(env, val));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_string_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   char const* data = NULL;
   uint32_t len = 0;
   if (FIX_FAILED == fix_msg_get_string(msg, group, tagNum, &data, &len))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return enif_make_tuple2(env, ok_atom, enif_make_string_len(env, data, len, ERL_NIF_LATIN1));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_char_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   ERL_NIF_TERM group_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   FIXGroup* group = NULL;
   ERL_NIF_TERM res = get_parser_msg_group(env, argv[0], &parser_res, &msg_res, &group_res, &parser, &msg, &group);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t tagNum = 0;
   if (!enif_get_int(env, argv[1], &tagNum))
   {
      return make_error(env, "Wrong tag num.");
   }
   char val;
   if (FIX_FAILED == fix_msg_get_char(msg, group, tagNum, &val))
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   return enif_make_tuple2(env, ok_atom, enif_make_int(env, val));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM get_data_field(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   return make_error(env, "not impemented yet");
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM msg_to_str(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   ERL_NIF_TERM msg_res;
   FIXParser* parser = NULL;
   FIXMsg* msg = NULL;
   ERL_NIF_TERM res = get_parser_msg(env, argv[0], &parser_res, &msg_res, &parser, &msg);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t delimiter = 0;
   if (!enif_get_int(env, argv[1], &delimiter) || delimiter <= 0 || delimiter >= 255)
   {
      return make_error(env, "Wrong delimiter.");
   }
   uint32_t reqBuffLen = DEF_BINARY_SIZE;
   ErlNifBinary bin;
   if (!enif_alloc_binary(reqBuffLen, &bin))
   {
      return make_error(env, "Unable to allocate binary.");
   }
   if (FIX_FAILED == fix_msg_to_str(msg, (char)delimiter, (char*)bin.data, bin.size, &reqBuffLen))
   {
      if (reqBuffLen > bin.size) // realloc needed
      {
         if (!enif_realloc_binary(&bin, reqBuffLen))
         {
            res = make_error(env, "Unable to reallocate binary.");
         }
         if (FIX_FAILED == fix_msg_to_str(msg, (char)delimiter, (char*)bin.data, bin.size, &reqBuffLen))
         {
            res = make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
         }
      }
      else
      {
         res = make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
      }
   }
   if (res != ok_atom)
   {
      enif_release_binary(&bin);
      return res;
   }
   if (bin.size > reqBuffLen)
   {
      enif_realloc_binary(&bin, reqBuffLen);
   }
   ERL_NIF_TERM bin_term = enif_make_binary(env, &bin);
   enif_release_binary(&bin);
   return enif_make_tuple2(env, ok_atom, bin_term);
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ERL_NIF_TERM str_to_msg(ErlNifEnv* env, int32_t argc, ERL_NIF_TERM const argv[])
{
   ERL_NIF_TERM parser_res;
   FIXParser* parser = NULL;
   ERL_NIF_TERM res = get_parser(env, argv[0], &parser_res, &parser);
   if (res != ok_atom)
   {
      return res;
   }
   int32_t delimiter = 0;
   if (!enif_get_int(env, argv[1], &delimiter) || delimiter <= 0 || delimiter >= 255)
   {
      return make_error(env, "Wrong delimiter.");
   }
   ErlNifBinary bin;
   if (!enif_inspect_binary(env, argv[2], &bin))
   {
      return make_error(env, "Wrong binary.");
   }
   char const* stop = NULL;
   FIXMsg* fix_msg = fix_parser_str_to_msg(parser, (char const*)bin.data, bin.size, delimiter, &stop);
   if (!fix_msg)
   {
      return make_parser_error(env, fix_parser_get_error_code(parser), fix_parser_get_error_text(parser));
   }
   FIXMsg** pfix_msg = (FIXMsg**)enif_alloc_resource(message_res, sizeof(FIXMsg*));
   *pfix_msg = fix_msg;
   ERL_NIF_TERM pfix_msg_term = enif_make_resource(env, pfix_msg);
   enif_release_resource(pfix_msg);
   uint32_t pos = stop - (char const*)bin.data + 1;
   char const* msgType = NULL;
   uint32_t len = 0;
   fix_msg_get_string(fix_msg, NULL, FIXFieldTag_MsgType, &msgType, &len);
   return enif_make_tuple3(
         env,
         ok_atom,
         enif_make_tuple3(env,
            msg_atom,
            enif_make_string_len(env, msgType, len, ERL_NIF_LATIN1),
            enif_make_tuple2(env, parser_res, pfix_msg_term)),
         enif_make_sub_binary(env, argv[2], pos, bin.size - pos));
}

/*-----------------------------------------------------------------------------------------------------------------------*/
static ErlNifFunc nif_funcs[] =
{
   {"create",            3, create           },
   {"create_msg",        2, create_msg       },
   {"add_group",         2, add_group        },
   {"get_group",         3, get_group        },
   {"del_group",         3, del_group        },
   {"set_int32_field",   3, set_int32_field  },
   {"set_int64_field",   3, set_int64_field  },
   {"set_double_field",  3, set_double_field },
   {"set_string_field",  3, set_string_field },
   {"set_char_field",    3, set_char_field   },
   {"set_data_field",    3, set_data_field   },
   {"get_int32_field",   2, get_int32_field  },
   {"get_int64_field",   2, get_int64_field  },
   {"get_double_field",  2, get_double_field },
   {"get_string_field",  2, get_string_field },
   {"get_char_field",    2, get_char_field   },
   {"get_data_field",    2, get_data_field   },
   {"msg_to_str",        2, msg_to_str       },
   {"str_to_msg",        3, str_to_msg       }
};

ERL_NIF_INIT(fix_parser, nif_funcs, load, NULL, NULL, NULL)
