-module(gen_fix_session).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([send_logon/4]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

send_logon(SessionID, Socket, Logon, Timeout) ->
   case catch gen_server:call(SessionID, {logon, Socket, Logon}, Timeout) of
      ok ->
         ok;
      {'EXIT', {Reason, _}} ->
         {error, Reason}
   end.

start_link(Args = #fix_session_acceptor_config{senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   gen_server:start_link({local, SessionID}, ?MODULE, Args);

start_link(Args = #fix_session_initiator_config{senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   gen_server:start_link({local, SessionID}, ?MODULE, Args).

init(_Args) ->
   {ok, #state{}}.

handle_call({logon, Socket, LogonBin}, _From, State) ->
   {reply, ok, State};

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
