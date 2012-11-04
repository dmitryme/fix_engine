-module(erlang_fix).

-export([create_parser/3]).

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
-type parserRef() :: {pos_integer(), binary()}.

load_lib() ->
   erlang:load_nif(code:priv_dir(erlang_fix) ++ "/erlang_fix", 0).

-spec create_parser(string(), attrs(), flags()) -> {ok, parserRef()} | {error, string()}.
create_parser(_Path, _Attrs, _Flags) ->
   {error, library_not_loaded}.
