-module(fix_parser).

-export([create/3, create_msg/2, add_group/2, get_group/3, del_group/3,
      set_int32_field/3, set_int64_field/3, set_double_field/3, set_string_field/3, set_char_field/3,
      get_int32_field/2, get_int64_field/2, get_double_field/2, get_string_field/2, get_char_field/2]).

-on_load(load_lib/0).

-type attr()      :: {'page_size', pos_integer()} |
                     {'max_page_size', pos_integer()} |
                     {'num_pages', pos_integer()} |
                     {'max_pages', pos_integer()} |
                     {'num_groups', pos_integer()} |
                     {'max_groups', pos_integer()}.
-type attrs()     :: [attr()].
-type flag()      :: check_crc |
                     check_required |
                     check_value |
                     check_unknown_fields |
                     check_all.
-type flags()     :: [flag()].
-type parserRef() :: {reference(), binary()}.
-type msgRef()    :: {reference(), binary(), binary()}.
-type groupRef()  :: {reference(), binary(), binary(), binary()}.
-type ref()       :: msgRef() | groupRef().
-type tagNum()    :: pos_integer().
-type reason()    :: atom() |
                     string() |
                     {pos_integer(), string()}.

load_lib() ->
   erlang:load_nif(code:priv_dir(erlang_fix) ++ "/fix_parser", 0).

-spec create(string(), attrs(), flags()) -> {ok, parserRef()} | {error, reason()}.
create(_Path, _Attrs, _Flags) ->
   {error, library_not_loaded}.

-spec create_msg(parserRef(), string()) -> {ok, msgRef()} | {error, reason()}.
create_msg(_ParserRef, _MsgType) ->
   {error, library_not_loaded}.

-spec add_group(ref(), tagNum()) -> {ok, groupRef()} | {error, reason()}.
add_group(_Ref, _TagNum) ->
   {error, library_not_loaded}.

-spec get_group(ref(), tagNum(), pos_integer()) -> {ok, groupRef()} | {error, reason()}.
get_group(_Ref, _TagNum, _Idx) ->
   {error, library_not_loaded}.

-spec del_group(ref(), tagNum(), pos_integer()) -> {ok, groupRef()} | {error, reason()}.
del_group(_Ref, _TagNum, _Idx) ->
   {error, library_not_loaded}.

-spec set_int32_field(ref(), tagNum(), integer()) -> ok | {error, reason()}.
set_int32_field(_MsgRef, _TagNum, _Value) ->
   {error, library_not_loaded}.

-spec set_int64_field(ref(), tagNum(), integer()) -> ok | {error, reason()}.
set_int64_field(_MsgRef, _FieldNum, _Value) ->
   {error, library_not_loaded}.

-spec set_double_field(ref(), tagNum(), float()) -> ok | {error, reason()}.
set_double_field(_MsgRef, _TagNum, _Value) ->
   {error, library_not_loaded}.

-spec set_string_field(ref(), tagNum(), string()) -> ok | {error, reason()}.
set_string_field(_MsgRef, _TagNum, _Value) ->
   {error, library_not_loaded}.

-spec set_char_field(ref(), tagNum(), char()) -> ok | {error, reason()}.
set_char_field(_MsgRef, _TagNum, _Value) ->
   {error, library_not_loaded}.

-spec get_int32_field(ref(), tagNum()) -> {ok, integer()} | {error, reason()}.
get_int32_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_int64_field(ref(), tagNum()) -> {ok, integer()} | {error, reason()}.
get_int64_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_double_field(ref(), tagNum()) -> {ok, float()} | {error, reason()}.
get_double_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_string_field(ref(), tagNum()) -> {ok, string()} | {error, reason()}.
get_string_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_char_field(ref(), tagNum()) -> {ok, char()} | {error, reason()}.
get_char_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.
