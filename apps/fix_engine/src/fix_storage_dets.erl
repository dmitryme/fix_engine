-module(fix_storage_dets).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {table_ref}).

start_link(SessionCfg = #fix_session_config{storage = Storage}) ->
   gen_server:start_link({local, Storage}, ?MODULE, SessionCfg, []).

init(#fix_session_config{session_id = SessionID, storage_dir = Dir, storage_flags = Flags}) ->
   error_logger:info_msg("FLAGS: ~p", [Flags]),
   ok = fix_utils:make_dir(Dir),
   FileName = filename:join([Dir, SessionID]) ++ ".storage",
   TableName = fix_utils:list_to_atom("fix_storage_" ++ atom_to_list(SessionID) ++ "_table"),
   {ok, TableRef} = dets:open_file(TableName, [{file, FileName} | Flags]),
   error_logger:info_msg("[~p]: storage file '~p' has been opened.", [SessionID, FileName]),
   {ok, #state{table_ref = TableRef}}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast({store, MsgSeqNum, Msg}, State) ->
   ok = dets:insert(State#state.table_ref, {MsgSeqNum, Msg}),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
