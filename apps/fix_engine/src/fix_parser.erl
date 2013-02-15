-module(fix_parser).

-include("fix_parser.hrl").

-export([create/3, get_version/1, create_msg/2, add_group/2, get_group/3, del_group/3,
      set_int32_field/3, set_int64_field/3, set_double_field/3, set_string_field/3, set_char_field/3, set_data_field/3,
      get_int32_field/2, get_int32_field/3, get_int64_field/2, get_int64_field/3, get_double_field/2, get_double_field/3,
      get_string_field/2, get_string_field/3, get_char_field/2, get_char_field/3, get_data_field/2, get_data_field/3,
      get_session_id/2, msg_to_str/2, str_to_msg/3]).

-on_load(load_lib/0).

load_lib() ->
   erlang:load_nif(code:priv_dir(fix_engine) ++ "/fix_parser", 0).

-spec create(string(), attrs(), flags()) -> {ok, #parser{}} | {error, reason()}.
create(_Path, _Attrs, _Flags) ->
   {error, library_not_loaded}.

-spec get_version(#parser{}) -> string().
get_version(_ParserRef) ->
   {error, library_not_loaded}.

-spec create_msg(#parser{}, string()) -> {ok, #msg{}} | {error, reason()}.
create_msg(_ParserRef, _MsgType) ->
   {error, library_not_loaded}.

-spec add_group(ref(), tagNum()) -> {ok, #group{}} | {error, reason()}.
add_group(_Ref, _TagNum) ->
   {error, library_not_loaded}.

-spec get_group(ref(), tagNum(), pos_integer()) -> {ok, #group{}} | {error, reason()}.
get_group(_Ref, _TagNum, _Idx) ->
   {error, library_not_loaded}.

-spec del_group(ref(), tagNum(), pos_integer()) -> ok | {error, reason()}.
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

-spec set_data_field(ref(), tagNum(), binary()) -> ok | {error, reason()}.
set_data_field(_MsgRef, _TagNum, _Value) ->
   {error, library_not_loaded}.

-spec get_int32_field(ref(), tagNum()) -> {ok, integer()} | {error, reason()}.
get_int32_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_int32_field(ref(), tagNum(), integer()) -> {ok, integer()} | {error, reason()}.
get_int32_field(MsgRef, TagNum, DefValue) ->
   case get_int32_field(MsgRef, TagNum) of
      V = {ok, _Value} ->
         V;
      {error, ?FIX_ERROR_FIELD_NOT_FOUND, _} ->
         {ok, DefValue};
      Err ->
         Err
   end.

-spec get_int64_field(ref(), tagNum()) -> {ok, integer()} | {error, reason()}.
get_int64_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_int64_field(ref(), tagNum(), integer()) -> {ok, integer()} | {error, reason()}.
get_int64_field(MsgRef, TagNum, DefValue) ->
   case get_int64_field(MsgRef, TagNum) of
      V = {ok, _Value} ->
         V;
      {error, ?FIX_ERROR_FIELD_NOT_FOUND, _} ->
         {ok, DefValue};
      Err ->
         Err
   end.

-spec get_double_field(ref(), tagNum()) -> {ok, float()} | {error, reason()}.
get_double_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_double_field(ref(), tagNum(), float()) -> {ok, float()} | {error, reason()}.
get_double_field(MsgRef, TagNum, DefValue) ->
   case get_double_field(MsgRef, TagNum) of
      V = {ok, _Value} ->
         V;
      {error, ?FIX_ERROR_FIELD_NOT_FOUND, _} ->
         {ok, DefValue};
      Err ->
         Err
   end.

-spec get_string_field(ref(), tagNum()) -> {ok, string()} | {error, reason()}.
get_string_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_string_field(ref(), tagNum(), string()) -> {ok, string()} | {error, reason()}.
get_string_field(MsgRef, TagNum, DefValue) ->
   case get_string_field(MsgRef, TagNum) of
      V = {ok, _Value} ->
         V;
      {error, ?FIX_ERROR_FIELD_NOT_FOUND, _} ->
         {ok, DefValue};
      Err ->
         Err
   end.

-spec get_char_field(ref(), tagNum()) -> {ok, char()} | {error, reason()}.
get_char_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_char_field(ref(), tagNum(), char()) -> {ok, char()} | {error, reason()}.
get_char_field(MsgRef, TagNum, DefValue) ->
   case get_char_field(MsgRef, TagNum) of
      V = {ok, _Value} ->
         V;
      {error, ?FIX_ERROR_FIELD_NOT_FOUND, _} ->
         {ok, DefValue};
      Err ->
         Err
   end.

-spec get_data_field(ref(), tagNum()) -> {ok, binary()} | {error, reason()}.
get_data_field(_MsgRef, _TagNum) ->
   {error, library_not_loaded}.

-spec get_data_field(ref(), tagNum(), binary()) -> {ok, binary()} | {error, reason()}.
get_data_field(MsgRef, TagNum, DefValue) ->
   case get_data_field(MsgRef, TagNum) of
      V = {ok, _Value} ->
         V;
      {error, ?FIX_ERROR_FIELD_NOT_FOUND, _} ->
         {ok, DefValue};
      Err ->
         Err
   end.

-spec get_session_id(binary(), char()) -> {string(), string()}.
get_session_id(_BinData, _Delimiter) ->
   {error, library_not_loaded}.

-spec msg_to_str(#msg{}, char()) -> {ok, binary()} | {error, reason()}.
msg_to_str(_MsgRef, _Delimiter) ->
   {error, library_not_loaded}.

-spec str_to_msg(#parser{}, char(), binary()) -> {ok, #msg{}, binary()} | {error, reason()}.
str_to_msg(_ParserRef, _Delimiter, _BinData) ->
   {error, library_not_loaded}.
