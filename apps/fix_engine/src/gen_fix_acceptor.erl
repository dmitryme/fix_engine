-module(gen_fix_acceptor).

-include("fix_engine_config.hrl").
-include("fix_parser.hrl").
-include("fix_fields.hrl").

-behaviour(gen_server).

-export([set_socket/2, connect/1, disconnect/1]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(data, {session_id, socket = undef, useTracer = false, parser, seq_num_out = 1, se_num_in = 1, senderCompID, targetCompID, username,
      password, state = 'DISCONNECTED', heartbeat_int, timer_ref = undef, binary = <<>>}).

set_socket(SessionPid, Socket) ->
   gen_tcp:controlling_process(Socket, SessionPid),
   gen_server:call(SessionPid, {set_socket, Socket}).

connect(SessionID) ->
   gen_server:call(SessionID, connect).

disconnect(SessionID) ->
   gen_server:call(SessionID, disconnect).

start_link(Args = #fix_session_acceptor_config{senderCompID = SenderCompID, targetCompID = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("Starting acceptor session [~p].", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, []).

init(#fix_session_acceptor_config{fix_protocol = Protocol, fix_parser_flags = ParserFlags, senderCompID = SenderCompID, targetCompID = TargetCompID,
      username = Username, password = Password, useTracer = UseTracer}) ->
   case fix_parser:create(Protocol, [], ParserFlags) of
      {ok, ParserRef} -> error_logger:info_msg("Parser [~p] has been created.", [fix_parser:get_version(ParserRef)]);
      {error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   print_use_tracer(SessionID, UseTracer),
   {ok, #data{session_id = SessionID, parser = ParserRef, senderCompID = SenderCompID, targetCompID = TargetCompID,
         username = Username, password = Password, useTracer = UseTracer}}.

handle_call(connect, _From, Data = #data{state = 'DISCONNECTED'}) ->
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'CONNECTED']),
   {reply, ok, Data#data{state = 'CONNECTED'}};

handle_call(disconnect, _From, Data = #data{state = 'CONNECTED'}) ->
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'DISCONNECTED']),
   {reply, ok, Data#data{state = 'DISCONNECTED'}};

handle_call(disconnect, _From, Data = #data{state = 'LOGGED_IN'}) ->
   Msg = create_logout(Data#data.parser, "Explicitly disconnected"),
   send_fix_message(Msg, Data),
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'DISCONNECTED']),
   gen_tcp:close(Data#data.socket),
   {reply, ok, Data#data{socket = undef, seq_num_out = Data#data.seq_num_out + 1, state = 'DISCONNECTED'}};

handle_call({set_socket, Socket}, _From, Data = #data{state = 'CONNECTED'}) ->
   {reply, ok, Data#data{socket = Socket}};

handle_call(Request, _From, Data) ->
   error_logger:error_msg("Unsupported message [~p].", [Request]),
   {reply, ok, Data}.

handle_cast(_Request, Data) ->
   {noreply, Data}.

handle_info({timeout, _, heartbeat},  Data = #data{state = S}) when S =/= 'LOGGED_IN' ->
   {noreply, Data#data{timer_ref = undef}};

handle_info({timeout, _, heartbeat},  Data) ->
   {ok, Msg} = fix_parser:create_msg(Data#data.parser, "0"),
   send_fix_message(Msg, Data),
   TimerRef = erlang:start_timer(Data#data.heartbeat_int, self(), heartbeat),
   {noreply, Data#data{seq_num_out = Data#data.seq_num_out + 1, timer_ref = TimerRef}};

handle_info({tcp, _, <<>>}, Data) ->
   {noreply, Data};

handle_info({tcp, Socket, Bin}, Data = #data{socket = Socket, binary = PrefixBin, state = State})
      when State == 'CONNECTED' orelse State == 'LOGGED_IN' ->
   case fix_parser:str_to_msg(Data#data.parser, ?FIX_SOH, <<PrefixBin/binary, Bin/binary>>) of
      {ok, Msg, RestBin} ->
         trace(Msg, in, Data),
         Data1 = Data#data{binary = <<>>},
         Data2 = handle_msg(Msg, Data1),
         {noreply, NewData} = handle_info({tcp, Socket, RestBin}, Data2);
      {error, ?FIX_ERROR_BODY_TOO_SHORT, _} ->
         NewData = Data#data{binary = <<PrefixBin/binary, Bin/binary>>};
      {error, ErrCode, ErrText} ->
         error_logger:error_msg("Unable to parse incoming message. Error = [~p], Description = [~p].", [ErrCode,
               ErrText]),
         error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'DISCONNECTED']),
         gen_tcp:close(Data#data.socket),
         NewData = Data#data{socket = undef, state = 'DISCONNECTED'}
   end,
   if erlang:is_port(NewData#data.socket) -> inet:setopts(Socket, [{active, once}]);
      true -> ok
   end,
   {noreply, NewData};

handle_info({tcp_closed, Socket}, Data = #data{socket = Socket}) ->
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'CONNECTED']),
   {noreply, Data#data{socket = undef, state = 'CONNECTED'}};

handle_info(Info, Data) ->
   error_logger:error_msg("Unsupported message [~p].", [Info]),
   {noreply, Data}.

terminate(_Reason, _Data) ->
   ok.

code_change(_OldVsn, Data, _Extra) ->
   {ok, Data}.

handle_msg(Msg = #msg{type = "A"}, Data = #data{socket = Socket, state = 'CONNECTED'}) ->
   error_logger:info_msg("~p: logon received.", [Data#data.session_id]),
   try
      validate_logon(Msg, Data#data.username, Data#data.password),
      {ok, HeartBtInt} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_HeartBtInt),
      {ok, ResetSeqNum} = fix_parser:get_char_field(Msg, ?FIXFieldTag_ResetSeqNumFlag, $N),
      SeqNumOut = if ResetSeqNum == $Y -> 1; true -> Data#data.seq_num_out end,
      NewData = Data#data{socket = Socket, seq_num_out = SeqNumOut, heartbeat_int = HeartBtInt * 1000},
      LogonReply = create_logon(Data#data.parser, HeartBtInt, ResetSeqNum),
      send_fix_message(LogonReply, NewData),
      error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'LOGGED_IN']),
      TimerRef = restart_heartbeat(NewData),
      inet:setopts(Socket, [{active, once}]),
      NewData#data{state = 'LOGGED_IN', seq_num_out = SeqNumOut + 1, timer_ref = TimerRef}
   catch
      throw:{badmatch, {error, _, Reason}} ->
         LogoutMsg = create_logout(Data#data.parser, Reason),
         send_fix_message(LogoutMsg, Data),
         gen_tcp:close(Socket),
         Data;
      throw:{error, Reason} ->
         LogoutMsg = create_logout(Data#data.parser, Reason),
         send_fix_message(LogoutMsg, Data),
         gen_tcp:close(Socket),
         Data;
      _:Err ->
         error_logger:error_msg("Logon failed: ~p", [Err]),
         LogoutMsg = create_logout(Data#data.parser, "Logon failed"),
         send_fix_message(LogoutMsg, Data),
         gen_tcp:close(Socket),
         Data
   end;
handle_msg(#msg{type = "5"}, Data = #data{state = 'LOGGED_IN'}) ->
   Logout = create_logout(Data#data.parser, "Bye"),
   send_fix_message(Logout, Data),
   Data#data{seq_num_out = Data#data.seq_num_out + 1, timer_ref = undef};
handle_msg(TestRequestMsg = #msg{type = "1"}, Data = #data{seq_num_out = SeqNumOut, state = 'LOGGED_IN'}) ->
   {ok, TestReqID} = fix_parser:get_string_field(TestRequestMsg, ?FIXFieldTag_TestReqID),
   {ok, HeartbeatMsg} = fix_parser:create_msg(Data#data.parser, "0"),
   ok = fix_parser:set_string_field(HeartbeatMsg, ?FIXFieldTag_TestReqID, TestReqID),
   send_fix_message(HeartbeatMsg, Data),
   Data#data{seq_num_out = SeqNumOut + 1, timer_ref = restart_heartbeat(Data)};
handle_msg(_Msg, Data) ->
   Data.

restart_heartbeat(#data{timer_ref = undef, heartbeat_int = Timeout}) ->
   erlang:start_timer(Timeout, self(), heartbeat);
restart_heartbeat(#data{timer_ref = OldTimerRef, heartbeat_int = Timeout}) ->
   erlang:cancel_timer(OldTimerRef),
   erlang:start_timer(Timeout, self(), heartbeat).

validate_logon(Msg = #msg{type = "A"}, Username, Password) ->
   {ok, Username1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Username, ""),
   {ok, Password1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Password, ""),
   if ((Username == Username1) orelse (Password == Password1)) -> ok;
      true ->
         throw({error, "Wrong Username/Password"})
   end;
validate_logon(_, _, _) ->
   throw({error, "Not a logon message"}).

create_logout(Parser, Text) ->
   {ok, Msg} = fix_parser:create_msg(Parser, "5"),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_Text, Text),
   Msg.

create_logon(Parser, HeartBtInt, ResetSeqNum) ->
   {ok, Msg} = fix_parser:create_msg(Parser, "A"),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_HeartBtInt, HeartBtInt),
   ok = fix_parser:set_char_field(Msg, ?FIXFieldTag_ResetSeqNumFlag, ResetSeqNum),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_EncryptMethod, 0),
   Msg.

send_fix_message(Msg, Data) ->
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SenderCompID, Data#data.senderCompID),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_TargetCompID, Data#data.targetCompID),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_MsgSeqNum, Data#data.seq_num_out),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SendingTime, fix_utils:now_utc()),
   {ok, BinMsg} = fix_parser:msg_to_str(Msg, ?FIX_SOH),
   ok = gen_tcp:send(Data#data.socket, BinMsg),
   trace(Msg, out, Data).

trace(Msg, Direction, #data{session_id = SID, useTracer = true}) ->
   fix_tracer:trace(SID, Direction, Msg);
trace(_, _, _) -> ok.

print_use_tracer(SessionID, true) ->
   error_logger:info_msg("Session [~p] will use tracer.", [SessionID]);
print_use_tracer(_SessionID, _) ->
   ok.
