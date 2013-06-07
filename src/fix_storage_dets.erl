-module(fix_storage_dets).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {storage_ref, metadata_ref}).

-define(def_metadata, [{seq_num_in, 0}, {seq_num_out, 0}, {correctly_terminated, false}]).

start_link(SessionCfg = #fix_session_config{storage = Storage}) ->
   gen_server:start_link({local, Storage}, ?MODULE, SessionCfg, []).

init(#fix_session_config{session_id = SessionID, storage_dir = Dir, storage_flags = Flags}) ->
   ok = fix_utils:make_dir(Dir),
   StorageTableRef = open_table(Dir, SessionID, "storage", Flags),
   MetTableRef = open_table(Dir, SessionID, "metadata", []),
   case dets:info(MetTableRef, size) of
      0 ->
         dets:insert(MetTableRef, ?def_metadata),
      Size when Size > 0 ->
         Items = dets:select(MetTableRef, [{{'_', '_'}, [], ['$_']}]),
         error_logger:info_msg("[~p] metadata : ~p", [Items])
   end,
   {ok, #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}}.

handle_call({get_metadata, Item}, _From, State = #state{metadata_ref = MetTableRef}) ->
   case dets:lookup(MetTableRef, Item) of
      [] ->
         {reply, {error, not_found}, State};
      [Value] ->
         {reply, {ok, Value}, State}
   end;

handle_cast(reset, State = #state{storage_ref = StorageTableRef}) ->
   ok = dets:delete_all_objects(StorageTableRef),
   ok = dets:insert(TableRef, ?def_metadata),
   {noreply, State};

handle_cast({seq_num_in, MsgSeqNum}, State = #state{metadata_ref = MetTableRef}) ->
   ok = dets:insert(MetTableRef, {seq_num_in, MsgSeqNum}),
   {noreply, State};

handle_cast({store, MsgSeqNum, Type, Msg}, State = #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   ok = dets:insert(StorageTableRef, {MsgSeqNum, Type, Msg}),
   ok = dets:insert(StorageTableRef, {seq_num_out, MsgSeqNum}),
   {noreply, State};

handle_cast({resend, From, BeginSeqNo, EndSeqNo}, State = #state{storage_ref = StorageTableRef, metadata_ref = MetTableRef}) ->
   [SeqNumOut] = dets:lookup(MetTableRef, seq_num_out),
   EndSeqNo1 = if (EndSeqNo == 0) -> SeqNumOut; true -> EndSeqNo end,
   error_logger:info_msg("[~p]: try to find messages [~p,~p].", [SessionID, BeginSeqNo, EndSeqNo1]),
   UnorderedMsgs = dets:select(TableRef,  [{{'$1', '_', '_'}, [
               {'>=', '$1', BeginSeqNo},{'=<', '$1', EndSeqNo1}], ['$_']}]),
   Msgs = lists:sort(UnorderedMsgs),
   error_logger:info_msg("[~p]: ~p messages will be resent.", [SessionID, length(Msgs)]),
   From ! {resend, Msgs},
   {noreply, State};

handle_cast({correctly_terminated, true}, State = #state{metadata_ref = MetTableRef}) ->
   ok = dets:insert(MetTableRef, {correctly_terminated, true}),
   {noreply, State}.

handle_cast({correctly_terminated, false}, State = #state{metadata_ref = MetTableRef}) ->
   ok = dets:insert(MetTableRef, {correctly_terminated, false}),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, #state{table_ref = TableRef}) ->
   ok = dets:close(TableRef),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

open_table(Dir, SessionID, Suffix, Flags) ->
   FileName = filename:join([Dir, SessionID]) ++ "." ++ Suffix,
   TableName = fix_utils:list_to_atom("fix_storage_" ++ atom_to_list(SessionID) ++ "_" ++ Suffix),
   {ok, TableRef} = dets:open_file(TableName, [{file, FileName} | Flags]),
   error_logger:info_msg("[~p]: file '~p' has been opened.", [SessionID, FileName]),
   TableRef.
