-module(fix_utils).

-export([list_to_atom/1,
      get_property/2,
      get_property/3,
      make_session_id/2,
      now_utc/0,
      unow_utc/0,
      now/0,
      unow/0,
      trace/3,
      reset/2,
      store/3,
      make_dir/1]).

list_to_atom(List) ->
   try erlang:list_to_existing_atom(List) of
      Atom -> Atom
   catch
      error:badarg -> erlang:list_to_atom(List)
   end.

get_property(Key, PropList) ->
   case proplists:get_value(Key, PropList) of
      undefined ->
         throw("Configuration parameter" ++ erlang:atom_to_list(Key) ++ " is missing.");
      Value ->
         Value
   end.

get_property(Key, PropList, DefValue) ->
   case proplists:get_value(Key, PropList) of
      undefined ->
         DefValue;
      Value ->
         Value
   end.

make_session_id(SenderCompID, TargetCompID) ->
   SessionID = SenderCompID ++ "_" ++ TargetCompID,
   ?MODULE:list_to_atom(SessionID).

now_utc() ->
   Now = {_, _, MicroSec} = erlang:now(),
   {{YYYY, M, DD},{HH, MM, SS}} = calendar:now_to_universal_time(Now),
   lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
         [YYYY, M, DD, HH, MM, SS, MicroSec div 1000])).

unow_utc() ->
   Now = {_, _, MicroSec} = erlang:now(),
   {{YYYY, M, DD},{HH, MM, SS}} = calendar:now_to_universal_time(Now),
   lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
         [YYYY, M, DD, HH, MM, SS, MicroSec])).

now() ->
   Now = {_, _, MicroSec} = erlang:now(),
   {{YYYY, M, DD},{HH, MM, SS}} = calendar:now_to_local_time(Now),
   lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
         [YYYY, M, DD, HH, MM, SS, MicroSec div 1000])).

unow() ->
   Now = {_, _, MicroSec} = erlang:now(),
   {{YYYY, M, DD},{HH, MM, SS}} = calendar:now_to_local_time(Now),
   lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B-~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0B",
         [YYYY, M, DD, HH, MM, SS, MicroSec])).

trace(Tracer, Direction, Msg) ->
   gen_server:cast(Tracer, {Direction, fix_utils:unow(), Msg}).

reset(Storage, SessionID) ->
   gen_server:cast(Storage, {reset, SessionID}).

store(Storage, MsgSeqNum, Msg) ->
   gen_server:cast(Storage, {store, MsgSeqNum, Msg}).

make_dir(Dir) ->
   case file:make_dir(Dir) of
      ok ->
         ok;
      {error, eexist} ->
         ok;
      Error ->
         exit(Error)
   end.
