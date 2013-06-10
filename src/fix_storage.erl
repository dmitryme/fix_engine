-module(fix_storage).

-export([
      reset/1,
      store_message/4,
      get_messages/3,
      get_metadata/2,
      set_metadata/3
      ]).

reset(Storage) ->
   gen_server:cast(Storage, reset).

store_message(Storage, MsgSeqNum, Type, Msg) ->
   gen_server:cast(Storage, {store_message, MsgSeqNum, Type, Msg}).

get_messages(Storage, BeginSeqNo, EndSeqNo) ->
   gen_server:cast(Storage, {get_messages, self(), BeginSeqNo, EndSeqNo}).

get_metadata(Storage, Item) ->
   gen_server:call(Storage, {get_metadata, Item}).

set_metadata(Storage, Item, Value) ->
   gen_server:call(Storage, {set_metadata, Item, Value}).
