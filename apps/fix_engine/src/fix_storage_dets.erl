-module(fix_storage_dets).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {table_ref, seq_num_in, seq_num_out}).

start_link(SessionCfg = #fix_session_config{storage = Storage}) ->
   gen_server:start_link({local, Storage}, ?MODULE, SessionCfg, []).

init(#fix_session_config{session_id = SessionID, storage_dir = Dir, storage_flags = Flags}) ->
   ok = fix_utils:make_dir(Dir),
   FileName = filename:join([Dir, SessionID]) ++ ".storage",
   TableName = fix_utils:list_to_atom("fix_storage_" ++ atom_to_list(SessionID) ++ "_table"),
   {ok, TableRef} = dets:open_file(TableName, [{file, FileName} | Flags]),
   error_logger:info_msg("[~p]: storage file '~p' has been opened.", [SessionID, FileName]),
   case dets:lookup(TableRef, stat) of
      [{stat, SeqNumIn, SeqNumOut}] ->
         ok;
      [] ->
         SeqNumIn = 0,
         SeqNumOut = 0,
         ok = dets:insert(TableRef, {stat, SeqNumIn, SeqNumOut})
   end,
   {ok, #state{table_ref = TableRef, seq_num_in = SeqNumIn, seq_num_out = SeqNumOut}}.

handle_call(get_stat, _From, State) ->
   {reply, {ok, {State#state.seq_num_in, State#state.seq_num_out}}, State}.

handle_cast(reset, State = #state{table_ref = TableRef}) ->
   ok = dets:delete_all_objects(TableRef),
   ok = dets:insert(TableRef, {stat, 0, 0}),
   {noreply, State#state{seq_num_in = 0, seq_num_out = 0}};

handle_cast({seq_num_in, MsgSeqNum}, State = #state{table_ref = TableRef, seq_num_out = SeqNumOut}) ->
   ok = dets:insert(TableRef, {stat, MsgSeqNum, SeqNumOut}),
   {noreply, State#state{seq_num_in = MsgSeqNum}};

handle_cast({store, MsgSeqNum, Msg}, State = #state{table_ref = TableRef, seq_num_in = SeqNumIn, seq_num_out = SeqNumOut}) ->
   ok = dets:insert(TableRef, {MsgSeqNum, Msg}),
   ok = dets:insert(TableRef, {stat, SeqNumIn, SeqNumOut}),
   {noreply, State#state{seq_num_out = MsgSeqNum}}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
