-module(fix_tracer).

-export([trace/3]).

trace(Tracer, Direction, Msg) ->
   gen_server:cast(Tracer, {Direction, fix_utils:unow(), Msg}).
