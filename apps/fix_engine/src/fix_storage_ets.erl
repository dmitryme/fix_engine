-module(fix_storage_ets).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {session_id, table_ref, seq_num_in, seq_num_out}).

start_link(SessionCfg = #fix_session_config{storage = Storage}) ->
   gen_server:start_link({local, Storage}, ?MODULE, SessionCfg, []).

init(#fix_session_config{session_id = SessionID, storage_dir = Dir, storage_flags = Flags}) ->
   ok = fix_utils:make_dir(Dir),
   TableName = fix_utils:list_to_atom("fix_storage_" ++ atom_to_list(SessionID) ++ "_table"),
   TableRef = ets:new(TableName, Flags),
   error_logger:info_msg("[~p]: storage file '~p' has been created.", [SessionID, TableName]),
   true = ets:insert(TableRef, {stat, 0, 0}),
   {ok, #state{session_id = SessionID, table_ref = TableRef, seq_num_in = 0, seq_num_out = 0}}.

handle_call(get_stat, _From, State) ->
   {reply, {ok, {State#state.seq_num_in, State#state.seq_num_out}}, State}.

handle_cast(reset, State = #state{table_ref = TableRef}) ->
   ok = ets:delete_all_objects(TableRef),
   true = ets:insert(TableRef, {stat, 0, 0}),
   {noreply, State#state{seq_num_in = 0, seq_num_out = 0}};

handle_cast({seq_num_in, MsgSeqNum}, State = #state{table_ref = TableRef, seq_num_out = SeqNumOut}) ->
   true = ets:insert(TableRef, {stat, MsgSeqNum, SeqNumOut}),
   {noreply, State#state{seq_num_in = MsgSeqNum}};

handle_cast({store, MsgSeqNum, Type, Msg}, State = #state{table_ref = TableRef, seq_num_in = SeqNumIn, seq_num_out = SeqNumOut}) ->
   true = ets:insert(TableRef, {MsgSeqNum, Type, Msg}),
   true = ets:insert(TableRef, {stat, SeqNumIn, SeqNumOut}),
   {noreply, State#state{seq_num_out = MsgSeqNum}};

handle_cast({resend, From, BeginSeqNo, EndSeqNo},
      State = #state{session_id = SessionID, table_ref = TableRef, seq_num_out = SeqNumOut}) ->
   EndSeqNo1 = if (EndSeqNo == 0) -> SeqNumOut; true -> EndSeqNo end,
   error_logger:info_msg("[~p]: try to find messages [~p,~p].", [SessionID, BeginSeqNo, EndSeqNo1]),
   UnorderedMsgs = ets:select(TableRef,  [{{'$1', '_', '_'}, [
               {'>=', '$1', BeginSeqNo},{'=<', '$1', EndSeqNo1}, {'=/=', '$1', stat}], ['$_']}]),
   Msgs = lists:sort(UnorderedMsgs),
   error_logger:info_msg("[~p]: ~p messages will be resent.", [SessionID, length(Msgs)]),
   From ! {resend, Msgs},
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, #state{table_ref = TableRef}) ->
   ok = ets:delete(TableRef),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.