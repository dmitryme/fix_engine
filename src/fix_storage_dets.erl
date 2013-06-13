-module(fix_storage_dets).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {session_id, storage_ref, metadata_ref}).

start_link(SessionCfg = #fix_session_config{storage = Storage}) ->
   gen_server:start_link({local, Storage}, ?MODULE, SessionCfg, []).

init(#fix_session_config{session_id = SessionID, storage_dir = Dir, storage_flags = Flags}) ->
   ok = fix_utils:make_dir(Dir),
   StorageTableRef = open_table(Dir, SessionID, "storage", Flags),
   MetTableRef = open_table(Dir, SessionID, "metadata", []),
   case dets:info(MetTableRef, size) of
      0 ->
         ok = dets:insert(MetTableRef, ?def_metadata);
      Size when Size > 0 ->
         Items = dets:select(MetTableRef, [{{'_', '_'}, [], ['$_']}]),
         error_logger:info_msg("[~p] metadata : ~p", [SessionID, Items])
   end,
   {ok, #state{session_id = SessionID, storage_ref = StorageTableRef, metadata_ref = MetTableRef}}.

handle_call({get_metadata, Item}, _From, State = #state{metadata_ref = MetTableRef}) ->
   case dets:lookup(MetTableRef, Item) of
      [] ->
         {reply, {error, not_found}, State};
      [{Item, Value}] ->
         {reply, {ok, Value}, State}
   end;

handle_call({set_metadata, Item, Value}, _From, State = #state{metadata_ref = MetTableRef}) ->
   ok = dets:insert(MetTableRef, {Item, Value}),
   {reply, ok, State}.

handle_cast(reset, State = #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   ok = dets:delete_all_objects(StorageTableRef),
   ok = dets:delete_all_objects(MetTableRef),
   ok = dets:insert(MetTableRef, ?def_metadata),
   {noreply, State};

handle_cast({store_message, MsgSeqNum, Type, Msg}, State = #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   ok = dets:insert(StorageTableRef, {MsgSeqNum, Type, Msg}),
   ok = dets:insert(MetTableRef, {seq_num_out, MsgSeqNum}),
   {noreply, State};

handle_cast({get_messages, From, BeginSeqNo, EndSeqNo}, State = #state{session_id = SessionID, storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   [SeqNumOut] = dets:lookup(MetTableRef, seq_num_out),
   EndSeqNo1 = if (EndSeqNo == 0) -> SeqNumOut; true -> EndSeqNo end,
   error_logger:info_msg("[~p]: try to find messages [~p,~p].", [SessionID, BeginSeqNo, EndSeqNo1]),
   UnorderedMsgs = dets:select(StorageTableRef,  [{{'$1', '_', '_'}, [
               {'>=', '$1', BeginSeqNo},{'=<', '$1', EndSeqNo1}], ['$_']}]),
   Msgs = lists:sort(UnorderedMsgs),
   error_logger:info_msg("[~p]: ~p messages will be resent.", [SessionID, length(Msgs)]),
   From ! {resend, Msgs},
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   ok = dets:close(StorageTableRef),
   ok = dets:close(MetTableRef),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

open_table(Dir, SessionID, Suffix, Flags) ->
   FileName = filename:join([Dir, SessionID]) ++ "." ++ Suffix,
   TableName = fix_utils:list_to_atom("fix_storage_" ++ atom_to_list(SessionID) ++ "_" ++ Suffix),
   {ok, TableRef} = dets:open_file(TableName, [{file, FileName} | Flags]),
   error_logger:info_msg("[~p]: file '~p' has been opened.", [SessionID, FileName]),
   TableRef.
