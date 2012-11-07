-module(fix_msg).

-export([create/2]).

-on_load(load_lib/0).

load_lib() ->
   erlang:load_nif(code:priv_dir(erlang_fix) ++ "/fix_msg", 0).

-type msgRef() :: {reference(), binary(), binary()}.
-type reason() :: {pos_integer(), string()}.

-spec create(fix_parser:parserRef(), string()) -> {ok, msgRef()} | {error, reason()}.
create(_ParserRef, _MsgType) ->
   {error, library_not_loaded}.