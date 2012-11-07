-module(fix_msg).

-export([create/2]).

-on_load(load_lib/0).

load_lib() ->
   erlang:load_nif(code:priv_dir(erlang_fix) ++ "/fix_msg", 0).

-spec create(string(), attrs(), flags()) -> {ok, msgRef()} | {error, reason()}.
create(parserRef(), string()) ->
   {error, library_not_loaded}.