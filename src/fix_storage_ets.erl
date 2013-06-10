-module(fix_storage_ets).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {session_id, storage_ref, metadata_ref}).

start_link(SessionCfg = #fix_session_config{storage = Storage}) ->
   gen_server:start_link({local, Storage}, ?MODULE, SessionCfg, []).

init(#fix_session_config{session_id = SessionID, storage_flags = Flags}) ->
   MetTableName = fix_utils:list_to_atom("fix_storage_" ++ atom_to_list(SessionID) ++ ".metadata"),
   MetTableRef = ets:new(MetTableName, Flags),
   error_logger:info_msg("[~p]: '~p' table has been created.", [SessionID, MetTableName]),
   StorageTableName = fix_utils:list_to_atom("fix_storage_" ++ atom_to_list(SessionID) ++ ".storage"),
   StorageTableRef = ets:new(StorageTableName, Flags),
   error_logger:info_msg("[~p]: '~p' table has been created.", [SessionID, StorageTableName]),
   true = ets:insert(MetTableRef, ?def_metadata),
   {ok, #state{session_id = SessionID, storage_ref = StorageTableRef, metadata_ref = MetTableRef}}.

handle_call({get_metadata, Item}, _From, State = #state{metadata_ref = MetTableRef}) ->
   case ets:lookup(MetTableRef, Item) of
      [] ->
         {reply, {error, not_found}, State};
      [{Item, Value}] ->
         {reply, {ok, Value}, State}
   end;

handle_call({set_metadata, Item, Value}, _From, State = #state{metadata_ref = MetTableRef}) ->
   true = ets:insert(MetTableRef, {Item, Value}),
   {reply, ok, State}.

handle_cast(reset, State = #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   true = ets:delete_all_objects(StorageTableRef),
   true = ets:delete_all_objects(MetTableRef),
   true = ets:insert(MetTableRef, ?def_metadata),
   {noreply, State};

handle_cast({store_message, MsgSeqNum, Type, Msg}, State = #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   true = ets:insert(StorageTableRef, {MsgSeqNum, Type, Msg}),
   true = ets:insert(MetTableRef, {seq_num_out, MsgSeqNum}),
   {noreply, State};

handle_cast({get_messages, From, BeginSeqNo, EndSeqNo}, State = #state{session_id = SessionID, storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   [SeqNumOut] = ets:lookup(MetTableRef, seq_num_out),
   EndSeqNo1 = if (EndSeqNo == 0) -> SeqNumOut; true -> EndSeqNo end,
   error_logger:info_msg("[~p]: try to find messages [~p,~p].", [SessionID, BeginSeqNo, EndSeqNo1]),
   UnorderedMsgs = ets:select(StorageTableRef,  [{{'$1', '_', '_'}, [
               {'>=', '$1', BeginSeqNo},{'=<', '$1', EndSeqNo1}], ['$_']}]),
   Msgs = lists:sort(UnorderedMsgs),
   error_logger:info_msg("[~p]: ~p messages will be resent.", [SessionID, length(Msgs)]),
   From ! {resend, Msgs},
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   ok = ets:delete(StorageTableRef),
   ok = ets:delete(MetTableRef),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
