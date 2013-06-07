-module(fix_storage).

-export([
      reset/1,
      store_msg_out/4,
      store_seq_num_in/2,
      get_stat/1,
      resend/3,
      get_metadata/2
      ]).

reset(Storage) ->
   gen_server:cast(Storage, reset).

store_msg_out(Storage, MsgSeqNum, Type, Msg) ->
   gen_server:cast(Storage, {store, MsgSeqNum, Type, Msg}).

store_seq_num_in(Storage, MsgSeqNum) ->
   gen_server:cast(Storage, {seq_num_in, MsgSeqNum}).

resend(Storage, BeginSeqNo, EndSeqNo) ->
   gen_server:cast(Storage, {resend, self(), BeginSeqNo, EndSeqNo}).

get_metadata(Storage, Item) ->
   gen_server:call(Storage, {get_metadata, Item}).
