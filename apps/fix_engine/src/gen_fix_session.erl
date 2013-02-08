-module(gen_fix_session).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([send_logon/4]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {session_id, parser}).

send_logon(SessionPid, Socket, Logon, Timeout) ->
   case catch gen_server:call(SessionPid, {logon, Socket, Logon}, Timeout) of
      ok ->
         ok;
      {'EXIT', {Reason, _}} ->
         {error, Reason}
   end.

start_link(Args = #fix_session_acceptor_config{senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("Starting acceptor session [~p].", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, []);

start_link(Args = #fix_session_initiator_config{senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("Starting initiator session [~p].", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, []).

init(#fix_session_acceptor_config{fix_protocol = Protocol, senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   case fix_parser:create(Protocol, [], []) of
      {ok, ParserRef} -> error_logger:info_msg("Parser [~p] has been created.", [fix_parser:get_version(ParserRef)]);
      {error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   {ok, #state{session_id = SessionID, parser = ParserRef}};

init(#fix_session_initiator_config{fix_protocol = Protocol, senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   case fix_parser:create(Protocol, [], []) of
      {ok, ParserRef} -> error_logger:info_msg("Parser [~p] has been created.", [fix_parser:get_version(ParserRef)]);
      {error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   {ok, #state{session_id = SessionID, parser = ParserRef}}.

handle_call({logon, Socket, LogonBin}, _From, State) ->
   error_logger:info_msg("~p: logon received.", [State#state.session_id]),
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
