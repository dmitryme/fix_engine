-module(gen_fix_acceptor).

-include("fix_engine_config.hrl").
-include("fix_parser.hrl").
-include("fix_fields.hrl").

-behaviour(gen_server).

-export([set_socket/2, connect/1, disconnect/1]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([apply/2, 'DISCONNECTED'/2, 'CONNECTED'/2, 'LOGGED_IN'/2]).

-record(data, {session_id, socket = undef, use_tracer = false, parser, seq_num_out = 1, se_num_in = 1, sender_comp_id, target_comp_id, username,
      password, state = 'DISCONNECTED', heartbeat_int, timer_ref = undef, binary = <<>>}).

set_socket(SessionPid, Socket) ->
   socket_controlling_process(Socket, SessionPid),
   gen_server:call(SessionPid, {gen_fix_acceptor, {set_socket, Socket}}).

connect(SessionID) ->
   gen_server:call(SessionID, {gen_fix_acceptor, connect}).

disconnect(SessionID) ->
   gen_server:call(SessionID, {gen_fix_acceptor, disconnect}).

start_link(Args = #fix_session_acceptor_config{sender_comp_id = SenderCompID, target_comp_id = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("Starting acceptor session [~p].", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, []).

init(#fix_session_acceptor_config{fix_protocol = Protocol, fix_parser_flags = ParserFlags, sender_comp_id = SenderCompID, target_comp_id = TargetCompID,
      username = Username, password = Password, use_tracer = UseTracer}) ->
   case fix_parser:create(Protocol, [], ParserFlags) of
      {ok, ParserRef} -> error_logger:info_msg("Parser [~p] has been created.", [fix_parser:get_version(ParserRef)]);
      {fix_error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   print_use_tracer(SessionID, UseTracer),
   {ok, #data{session_id = SessionID, parser = ParserRef, sender_comp_id = SenderCompID, target_comp_id = TargetCompID,
         username = Username, password = Password, use_tracer = UseTracer}}.

handle_call({gen_fix_acceptor, {set_socket, Socket}}, _From, Data) ->
   {reply, ok, Data#data{socket = Socket}};
handle_call({gen_fix_acceptor, connect}, _From, Data) ->
   {ok, NewData} = ?MODULE:apply(Data, connect),
   {reply, ok, NewData};
handle_call({gen_fix_acceptor, disconnect}, _From, Data) ->
   {ok, NewData} = ?MODULE:apply(Data, disconnect),
   {reply, ok, NewData};
handle_call(_Msg, _From, Data) ->
   % TODO: will call derived module
   {reply, ok, Data}.

handle_cast(_Request, Data) ->
   % TODO: will call derived module
   {noreply, Data}.

handle_info({tcp, Socket, Bin}, Data = #data{socket = Socket}) ->
   {ok, NewData} = parse_binary(Bin, Data),
   if erlang:is_port(NewData#data.socket) -> inet:setopts(Socket, [{active, once}]);
      true -> ok
   end,
   {noreply, NewData};
handle_info({tcp_closed, Socket}, Data = #data{socket = Socket}) ->
   {ok, NewData} = ?MODULE:apply(Data, tcp_closed),
   {noreply, NewData};
handle_info({timeout, _, heartbeat}, Data) ->
   {ok, NewData} = ?MODULE:apply(Data, timeout),
   {noreply, NewData}.

terminate(_Reason, _Data) ->
   ok.

code_change(_OldVsn, Data, _Extra) ->
   {ok, Data}.

% =====================================================================================================
% =========================== Acceptor FSM begin ======================================================
% =====================================================================================================

'DISCONNECTED'(connect, Data) ->
   {ok, Data#data{state = 'CONNECTED'}};

'DISCONNECTED'(#msg{type = "A"}, Data) ->
   error_logger:error_msg("Session [~p] not connected. Do connect first.", [Data#data.session_id]),
   socket_close(Data#data.socket),
   {ok, Data#data{socket = undef}};

'DISCONNECTED'({fix_error, _ErrCode, _ErrText}, Data) ->
   error_logger:error_msg("Session [~p] not connected. Do connect first.", [Data#data.session_id]),
   socket_close(Data#data.socket),
   {ok, Data#data{socket = undef}};

'DISCONNECTED'(timeout, Data) ->
   {ok, Data#data{timer_ref = undef}}.

'CONNECTED'(timeout, Data) ->
   {ok, Data#data{timer_ref = undef}};

'CONNECTED'(disconnect, Data) ->
   {ok, Data#data{state = 'DISCONNECTED'}};

'CONNECTED'({fix_error, ErrCode, ErrText}, Data) ->
   error_logger:error_msg("Unable to parse message. ErrCode = [~p], ErrText = [~p].", [ErrCode, ErrText]),
   socket_close(Data#data.socket),
   {ok, Data#data{socket = undef}};

'CONNECTED'(Msg = #msg{type = "A"}, Data = #data{socket = Socket, state = 'CONNECTED'}) ->
   error_logger:info_msg("~p: logon received.", [Data#data.session_id]),
   try
      validate_logon(Msg, Data#data.username, Data#data.password),
      {ok, HeartBtInt} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_HeartBtInt),
      {ok, ResetSeqNum} = fix_parser:get_char_field(Msg, ?FIXFieldTag_ResetSeqNumFlag, $N),
      SeqNumOut = if ResetSeqNum == $Y -> 1; true -> Data#data.seq_num_out end,
      NewData = Data#data{socket = Socket, seq_num_out = SeqNumOut, heartbeat_int = HeartBtInt * 1000},
      LogonReply = create_logon(Data#data.parser, HeartBtInt, ResetSeqNum),
      send_fix_message(LogonReply, NewData),
      {ok, NewData#data{state = 'LOGGED_IN', seq_num_out = SeqNumOut + 1, timer_ref = restart_heartbeat(NewData)}}
   catch
      throw:{badmatch, {fix_error, _, Reason}} ->
         LogoutMsg = create_logout(Data#data.parser, Reason),
         send_fix_message(LogoutMsg, Data),
         socket_close(Socket),
         {ok, Data#data{state = 'CONNECTED'}};
      throw:{error, Reason} ->
         LogoutMsg = create_logout(Data#data.parser, Reason),
         send_fix_message(LogoutMsg, Data),
         socket_close(Socket),
         {ok, Data#data{state = 'CONNECTED'}};
      _:Err ->
         error_logger:error_msg("Logon failed: ~p", [Err]),
         LogoutMsg = create_logout(Data#data.parser, "Logon failed"),
         send_fix_message(LogoutMsg, Data),
         socket_close(Socket),
         {ok, Data#data{state = 'CONNECTED'}}
   end.

'LOGGED_IN'(timeout, Data) ->
   {ok, Msg} = fix_parser:create_msg(Data#data.parser, "0"),
   send_fix_message(Msg, Data),
   TimerRef = erlang:start_timer(Data#data.heartbeat_int, self(), heartbeat),
   {ok, Data#data{seq_num_out = Data#data.seq_num_out + 1, timer_ref = TimerRef}};

'LOGGED_IN'(disconnect, Data) ->
   Msg = create_logout(Data#data.parser, "Explicitly disconnected"),
   send_fix_message(Msg, Data),
   socket_close(Data#data.socket),
   {ok, Data#data{socket = undef, seq_num_out = Data#data.seq_num_out + 1, state = 'DISCONNECTED'}};

'LOGGED_IN'(tcp_closed, Data) ->
   {ok, Data#data{socket = undef, state = 'CONNECTED'}};

'LOGGED_IN'(#msg{type = "0"}, Data) ->
   {ok, Data};

'LOGGED_IN'(#msg{type = "5"}, Data) ->
   Logout = create_logout(Data#data.parser, "Bye"),
   send_fix_message(Logout, Data),
   {ok, Data#data{seq_num_out = Data#data.seq_num_out + 1, timer_ref = undef}};

'LOGGED_IN'(TestRequestMsg = #msg{type = "1"}, Data = #data{seq_num_out = SeqNumOut}) ->
   {ok, TestReqID} = fix_parser:get_string_field(TestRequestMsg, ?FIXFieldTag_TestReqID),
   {ok, HeartbeatMsg} = fix_parser:create_msg(Data#data.parser, "0"),
   ok = fix_parser:set_string_field(HeartbeatMsg, ?FIXFieldTag_TestReqID, TestReqID),
   send_fix_message(HeartbeatMsg, Data),
   {ok, Data#data{seq_num_out = SeqNumOut + 1, timer_ref = restart_heartbeat(Data)}}.

% =====================================================================================================
% =========================== Acceptor FSM end ========================================================
% =====================================================================================================

apply(Data = #data{session_id = SessionID, state = OldState}, Msg) ->
   case (catch erlang:apply(?MODULE, OldState, [Msg, Data])) of
      {ok, NewData = #data{state = NewState}} ->
         if NewState =/= OldState ->
               error_logger:info_msg("Session [~p] state changed [~p]->[~p].", [SessionID, OldState, NewState]);
            true -> ok
         end,
         {ok, NewData};
      {'EXIT', {function_clause, [_]}} ->
         error_logger:warning_msg("Unsupported session [~p] state [~p] message [~p].", [SessionID, OldState, Msg]),
         {ok, Data};
      Other ->
         error_logger:error_msg("Wrong call [~p]", [Other]),
         exit({error_wrong_result, Other})
   end.

parse_binary(<<>>, Data) ->
   {ok, Data};
parse_binary(Bin, Data = #data{binary = PrefixBin}) ->
   case fix_parser:binary_to_msg(Data#data.parser, ?FIX_SOH, <<PrefixBin/binary, Bin/binary>>) of
      {ok, Msg, RestBin} ->
         trace(Msg, in, Data),
         Data1 = Data#data{binary = <<>>},
         {ok, Data2} = ?MODULE:apply(Data1, Msg),
         {ok, NewData} = parse_binary(RestBin, Data2);
      {fix_error, ?FIX_ERROR_BODY_TOO_SHORT, _} ->
         NewData = Data#data{binary = <<PrefixBin/binary, Bin/binary>>};
      {fix_error, ErrCode, ErrText} ->
         error_logger:error_msg("Unable to parse incoming message. Error = [~p], Description = [~p].", [ErrCode,
               ErrText]),
         {ok, NewData} = ?MODULE:apply(Data, {fix_error, ErrCode, ErrText})
   end,
   {ok, NewData}.

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
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SenderCompID, Data#data.sender_comp_id),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_TargetCompID, Data#data.target_comp_id),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_MsgSeqNum, Data#data.seq_num_out),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SendingTime, fix_utils:now_utc()),
   {ok, BinMsg} = fix_parser:msg_to_binary(Msg, ?FIX_SOH),
   ok = socket_send(Data#data.socket, BinMsg),
   trace(Msg, out, Data).

trace(Msg, Direction, #data{session_id = SID, use_tracer = true}) ->
   fix_tracer:trace(SID, Direction, Msg);
trace(_, _, _) -> ok.

print_use_tracer(SessionID, true) ->
   error_logger:info_msg("Session [~p] will use tracer.", [SessionID]);
print_use_tracer(_SessionID, _) ->
   ok.

socket_close(Socket) when is_port(Socket) ->
   gen_tcp:close(Socket);
socket_close(Socket) ->
   Socket ! tcp_close.

socket_send(Socket, Msg) when is_port(Socket) ->
   gen_tcp:send(Socket, Msg);
socket_send(Socket, Msg) ->
   Socket ! Msg.

socket_controlling_process(Socket, SessionPid) when is_port(Socket) ->
   gen_tcp:controlling_process(Socket, SessionPid);
socket_controlling_process(_Socket, _SessionPid) ->
   ok.
