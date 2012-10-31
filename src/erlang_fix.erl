-module(erlang_fix).

-export([create_parser/3]).

-on_load(load_lib/0).

load_lib() ->
   erlang:load_nif(code:priv_dir(erlang_fix) ++ "/erlang_fix", 0).

create_parser(_Path, _Attrs, _Flags) ->
   {error, library_not_loaded}.
