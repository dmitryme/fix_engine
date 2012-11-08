-module(fix_parser).

-export([create/3, create_msg/2, add_field/3]).

-on_load(load_lib/0).

-type attr() :: {'page_size', pos_integer()} |
                {'max_page_size', pos_integer()} |
                {'num_pages', pos_integer()} |
                {'max_pages', pos_integer()} |
                {'num_groups', pos_integer()} |
                {'max_groups', pos_integer()}.
-type attrs() :: [attr()].
-type flag() :: check_crc |
                 check_required |
                 check_value |
                 check_unknown_fields |
                 check_all.
-type flags() :: [flag()].
-type parserRef() :: {reference(), binary()}.
-type msgRef() :: {reference(), binary(), binary()}.
-type reason() :: atom() |
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

-spec add_field(msgRef(), pos_integer(), any()) -> ok | {error, reason()}.
add_field(_MsgRef, _FieldNum, _Value) ->
   {error, library_not_loaded}.