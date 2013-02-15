-module(gen_fix_session).

-include("fix_engine_config.hrl").
-include("fix_parser.hrl").
-include("fix_fields.hrl").

-behaviour(gen_server).

-export([send_logon/4, connect/1, disconnect/1]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(data, {session_id, socket = undef, parser, seq_num_out = 1, se_num_in = 1, senderCompID, targetCompID, username,
      password, state = 'DISCONNECTED'}).

send_logon(SessionPid, Socket, Logon, Timeout) ->
   gen_server:call(SessionPid, {logon, Socket, Logon}, Timeout).

connect(SessionID) ->
   gen_server:call(SessionID, connect).

disconnect(SessionID) ->
   gen_server:call(SessionID, disconnect).

start_link(Args = #fix_session_acceptor_config{senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("Starting acceptor session [~p].", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, []);

start_link(Args = #fix_session_initiator_config{senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("Starting initiator session [~p].", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, []).

init(#fix_session_acceptor_config{fix_protocol = Protocol, senderCompID = SenderCompID, targetCompID = TargetCompID,
      username = Username, password = Password}) ->
   case fix_parser:create(Protocol, [], []) of
      {ok, ParserRef} -> error_logger:info_msg("Parser [~p] has been created.", [fix_parser:get_version(ParserRef)]);
      {error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   {ok, #data{session_id = SessionID, parser = ParserRef, senderCompID = SenderCompID, targetCompID = TargetCompID,
         username = Username, password = Password}};

init(#fix_session_initiator_config{fix_protocol = Protocol, senderCompID = SenderCompID, targetCompID = TargetCompID,
      username = Username, password = Password}) ->
   case fix_parser:create(Protocol, [], []) of
      {ok, ParserRef} -> error_logger:info_msg("Parser [~p] has been created.", [fix_parser:get_version(ParserRef)]);
      {error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   {ok, #data{session_id = SessionID, parser = ParserRef, senderCompID = SenderCompID, targetCompID = TargetCompID,
         username = Username, password = Password}}.

handle_call(connect, _From, Data = #data{state = 'DISCONNECTED'}) ->
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'CONNECTED']),
   {reply, ok, Data#data{state = 'CONNECTED'}};

handle_call(disconnect, _From, Data = #data{state = 'CONNECTED'}) ->
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'DISCONNECTED']),
   {reply, ok, Data#data{state = 'DISCONNECTED'}};

handle_call(disconnect, _From, Data = #data{state = 'LOGGED_IN'}) ->
   Msg = create_logout(Data#data.parser, "Explicitly disconnected"),
   send_fix_message(Data#data.socket, Msg, Data#data.seq_num_out, Data#data.senderCompID, Data#data.targetCompID),
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'DISCONNECTED']),
   gen_tcp:close(Data#data.socket),
   {reply, ok, Data#data{socket = undef, seq_num_out = Data#data.seq_num_out + 1, state = 'DISCONNECTED'}};

handle_call({logon, Socket, LogonBin}, _From, Data = #data{state = 'CONNECTED'}) ->
   error_logger:info_msg("~p: logon received.", [Data#data.session_id]),
   case fix_parser:str_to_msg(Data#data.parser, ?FIX_SOH, LogonBin) of
      {ok, Msg, <<>>} ->
         Valid = validate_logon(Msg, Data#data.username, Data#data.password);
      {error, ErrCore, ErrMsg} ->
         Valid = false,
         error_logger:error_msg("Unable to parse logon message. Error = [~p], Message = [~p].", [ErrCore, ErrMsg])
   end,
   case Valid of
      true ->
         % TODO: process Logon here
         error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'LOGGED_IN']),
         {reply, ok, Data#data{socket = Socket, state = 'LOGGED_IN'}};
      false ->
         LogoutMsg = create_logout(Data#data.parser, "Logon failed"),
         send_fix_message(Socket, LogoutMsg, Data#data.seq_num_out, Data#data.senderCompID, Data#data.targetCompID),
         gen_tcp:close(Socket),
         {reply, ok, Data}
   end;

handle_call(Request, _From, Data) ->
   error_logger:error_msg("Unsupported message [~p].", [Request]),
   {reply, ok, Data}.

handle_cast(_Request, Data) ->
   {noreply, Data}.

handle_info(_Info, Data) ->
   {noreply, Data}.

terminate(_Reason, _Data) ->
   ok.

code_change(_OldVsn, Data, _Extra) ->
   {ok, Data}.

validate_logon(Msg = #msg{type = "A"}, Username, Password) ->
   {ok, Username1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Username, ""),
   {ok, Password1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Password, ""),
   if ((Username =/= Username1) orelse (Password =/= Password1)) -> false;
      true -> true
   end;
validate_logon(_, _, _) ->
   false.

create_logout(Parser, Text) ->
   case fix_parser:create_msg(Parser, "5") of
      {ok, Msg} ->
         ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_Text, Text),
         Msg;
      {error, ErrCode, ErrText} ->
         error_logger:error_msg("Unable to create logout message. Error = [~p], Message = [~p].", [ErrCode, ErrText]),
         error
   end.

send_fix_message(_Socket, error, _MsgSeqNum, _SenderCompID, _TargetCompID) ->
   ok;
send_fix_message(Socket, Msg, MsgSeqNum, SenderCompID, TargetCompID) ->
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SenderCompID, SenderCompID),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_TargetCompID, TargetCompID),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_MsgSeqNum, MsgSeqNum),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SendingTime, fix_utils:now_utc()),
   {ok, BinMsg} = fix_parser:msg_to_str(Msg, ?FIX_SOH),
   ok = gen_tcp:send(Socket, BinMsg).
