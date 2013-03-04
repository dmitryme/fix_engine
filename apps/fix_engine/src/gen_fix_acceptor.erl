-module(gen_fix_acceptor).

-include("fix_engine_config.hrl").
-include("fix_parser.hrl").
-include("fix_fields.hrl").

-behaviour(gen_server).

-export([set_socket/2, connect/1, disconnect/1, get_current_state/1, send_fix/2]).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([apply/2, 'DISCONNECTED'/2, 'CONNECTED'/2, 'LOGGED_IN'/2]).

-record(data, {module      :: atom(),
      module_state         :: term(),
      session_id           :: atom(),
      socket = undef       :: undef | port() | gen_tcp:socket(),
      use_tracer = false   :: true | false,
      parser               :: #parser{},
      seq_num_out = 1      :: pos_integer(),
      se_num_in = 1        :: pos_integer(),
      sender_comp_id       :: string(),
      target_comp_id       :: string(),
      username             :: string(),
      password             :: string(),
      state = 'DISCONNECTED' :: atom(),
      heartbeat_int        :: pos_integer(),
      timer_ref = undef    :: reference(),
      binary = <<>>        :: binary()}).

-callback init(atom(), #parser{}, term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {connect, State :: term()} |
    {connect, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_fix(FIXMessage :: #msg{}, State :: term()) ->
   {reply, FIXMessage :: #msg{}, NewState :: term()} |
   {noreply, NewState :: term()}.

set_socket(SessionPid, Socket) ->
   case gen_server:call(SessionPid, {gen_fix_acceptor, {set_socket, Socket}}) of
      Err = {error, _Reason} ->
         Err;
      ok ->
         socket_controlling_process(Socket, SessionPid),
         ok
   end.

connect(SessionID) ->
   gen_server:call(SessionID, {gen_fix_acceptor, connect}).

disconnect(SessionID) ->
   gen_server:call(SessionID, {gen_fix_acceptor, disconnect}).

get_current_state(SessionID) ->
   gen_server:call(SessionID, {gen_fix_acceptor, get_current_state}).

send_fix(SessionID, FixMsg) ->
   gen_server:call(SessionID, {gen_fix_acceptor, send_fix, FixMsg}).

start_link(Args = #fix_session_acceptor_config{sender_comp_id = SenderCompID, target_comp_id = TargetCompID}, Options) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("[~p]: starting.", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, Options).

init(#fix_session_acceptor_config{module = Module, module_args = MArgs, fix_protocol = Protocol, fix_parser_flags = ParserFlags,
      sender_comp_id = SenderCompID, target_comp_id = TargetCompID, username = Username, password = Password, use_tracer = UseTracer}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   case fix_parser:create(Protocol, [], ParserFlags) of
      {ok, ParserRef} -> error_logger:info_msg("[~p]: parser [~p] has been created.", [SessionID, fix_parser:get_version(ParserRef)]);
      {fix_error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   print_use_tracer(SessionID, UseTracer),
   Data = #data{module = Module, session_id = SessionID, parser = ParserRef, sender_comp_id = SenderCompID, target_comp_id = TargetCompID,
         username = Username, password = Password, use_tracer = UseTracer},
   case Module:init(SessionID, ParserRef, MArgs) of
      {ok, State} ->
         {ok, Data#data{module_state = State}};
      {ok, State, Timeout} ->
         {ok, Data#data{module_state = State}, Timeout};
      {connect, State} ->
         {ok, NewData} = ?MODULE:apply(Data, connect),
         {ok, NewData#data{module_state = State}};
      {connect, State, Timeout} ->
         {ok, NewData} = ?MODULE:apply(Data, connect),
         {ok, NewData#data{module_state = State}, Timeout};
      Other ->
         Other
   end.

handle_call({gen_fix_acceptor, {set_socket, _Socket}}, _From, Data = #data{socket = OldSocket}) when is_port(OldSocket) ->
   {reply, {error, already_connected}, Data};
handle_call({gen_fix_acceptor, {set_socket, Socket}}, _From, Data) ->
   {reply, ok, Data#data{socket = Socket}};
handle_call({gen_fix_acceptor, connect}, _From, Data) ->
   {ok, NewData} = ?MODULE:apply(Data, connect),
   {reply, ok, NewData};
handle_call({gen_fix_acceptor, disconnect}, _From, Data) ->
   {ok, NewData} = ?MODULE:apply(Data, disconnect),
   {reply, ok, NewData};
handle_call({gen_fix_acceptor, get_current_state}, _From, Data) ->
   {reply, Data#data.state, Data};
handle_call({gen_fix_acceptor, send_fix, FixMsg}, _From, Data) ->
   {ok, NewData} = ?MODULE:apply(Data, {send_fix, FixMsg}),
   {reply, ok, NewData};
handle_call(Msg, From, Data = #data{module = Module, module_state = MState}) ->
   case Module:handle_call(Msg, From, MState) of
      {reply, Reply, NewMState} ->
         {reply, Reply, Data#data{module_state = NewMState}};
      {reply, Reply, NewMState, Param} ->
         {reply, Reply, Data#data{module_state = NewMState}, Param};
      {noreply, NewMState} ->
         {noreply, Data#data{module_state = NewMState}};
      {noreply, NewMState, Param} ->
         {noreply, Data#data{module_state = NewMState}, Param};
      {stop, Reason, Reply, NewMState} ->
         {stop, Reason, Reply, Data#data{module_state = NewMState}};
      {stop, Reason, NewMState} ->
         {stop, Reason, Data#data{module_state = NewMState}};
      Reply ->
         {stop, {bad_return_value, Reply}, Data}
   end.

handle_cast(Request, Data = #data{module = Module, module_state = MState}) ->
   case Module:handle_cast(Request, MState) of
      {noreply, NewMState} ->
         {noreply, Data#data{module_state = NewMState}};
      {noreply, NewMState, Param} ->
         {noreply, Data#data{module_state = NewMState}, Param};
      {stop, Reason, NewMState} ->
         {stop, Reason, Data#data{module_state = NewMState}};
      Reply ->
         {stop, {bad_return_value, Reply}, Data}
   end.

handle_info({tcp, Socket, Bin}, Data = #data{socket = Socket}) ->
   case parse_binary(Bin, Data) of
      {ok, NewData = #data{socket = S}} when is_port(S) ->
         inet:setopts(Socket, [{active, once}]),
         {noreply, NewData};
      {ok, NewData} ->
         {noreply, NewData};
      {stop, Reason, NewData} ->
         {stop, Reason, NewData}
   end;
handle_info({tcp_closed, Socket}, Data = #data{socket = Socket}) ->
   {ok, NewData} = ?MODULE:apply(Data, tcp_closed),
   {noreply, NewData};
handle_info({timeout, _, heartbeat}, Data) ->
   {ok, NewData} = ?MODULE:apply(Data, timeout),
   {noreply, NewData};
handle_info(Info, Data = #data{module = Module, module_state = MState}) ->
   case Module:handle_info(Info, MState) of
      {noreply, NewMState} ->
         {noreply, Data#data{module_state = NewMState}};
      {noreply, NewMState, Param} ->
         {noreply, Data#data{module_state = NewMState}, Param};
      {stop, Reason, NewMState} ->
         {stop, Reason, Data#data{module_state = NewMState}};
      Reply ->
         {stop, {bad_return_value, Reply}, Data}
   end.

terminate(Reason, #data{module = Module, module_state = MState}) ->
   Module:terminate(Reason, MState).

code_change(OldVsn, Data  = #data{module = Module, module_state = MState}, Extra) ->
   case Module:code_change(OldVsn, MState, Extra) of
      {ok, NewMState} ->
         {ok, Data#data{module_state = NewMState}};
      Reply ->
         Reply
   end.

% =====================================================================================================
% =========================== Acceptor FSM begin ======================================================
% =====================================================================================================

% ================= DISCONNECTED BEGIN ================================================================
'DISCONNECTED'(connect, Data) ->
   {ok, Data#data{state = 'CONNECTED'}};

'DISCONNECTED'(timeout, Data) ->
   {ok, Data#data{timer_ref = undef}};

'DISCONNECTED'(_, Data) ->
   error_logger:error_msg("[~p]: not connected. Do connect first.", [Data#data.session_id]),
   socket_close(Data#data.socket),
   {ok, Data#data{socket = undef}}.
% ================= DISCONNECTED END ==================================================================

% ================= CONNECTED BEGIN ===================================================================
'CONNECTED'(timeout, Data) ->
   {ok, Data#data{timer_ref = undef}};

'CONNECTED'(disconnect, Data) ->
   {ok, Data#data{state = 'DISCONNECTED'}};

'CONNECTED'({fix_error, _ErrCode, ErrText}, Data) ->
   LogoutMsg = create_logout(Data#data.parser, ErrText),
   send_fix_message(LogoutMsg, Data),
   socket_close(Data#data.socket),
   {ok, Data#data{socket = undef, seq_num_out = Data#data.seq_num_out + 1}};

'CONNECTED'(Msg, Data = #data{socket = Socket, state = 'CONNECTED'}) ->
   error_logger:info_msg("[~p]: message [~p] received.", [Data#data.session_id, Msg#msg.type]),
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
      throw:{badmatch, {fix_error, _, ErrText}} ->
         LogoutMsg = create_logout(Data#data.parser, ErrText),
         send_fix_message(LogoutMsg, Data),
         socket_close(Socket),
         {ok, Data#data{state = 'CONNECTED'}};
      throw:{error, Reason} ->
         LogoutMsg = create_logout(Data#data.parser, Reason),
         send_fix_message(LogoutMsg, Data),
         socket_close(Socket),
         {ok, Data#data{state = 'CONNECTED'}};
      _:Err ->
         error_logger:error_msg("[~p]: logon failed: ~p", [Err]),
         LogoutMsg = create_logout(Data#data.parser, "Logon failed"),
         send_fix_message(LogoutMsg, Data),
         socket_close(Socket),
         {ok, Data#data{state = 'CONNECTED'}}
   end.

% ================= CONNECTED END =====================================================================

% ================= LOGGED_IN BEGIN ===================================================================
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

'LOGGED_IN'(#msg{type = "A"}, Data) ->
   error_logger:error_msg("[~p]: unexpected logon received.", [Data#data.session_id]),
   socket_close(Data#data.socket),
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
   {ok, Data#data{seq_num_out = SeqNumOut + 1, timer_ref = restart_heartbeat(Data)}};

'LOGGED_IN'(Msg = #msg{}, Data = #data{module = Module, module_state = MState}) ->
   case Module:handle_fix(Msg, MState) of
      {reply, ReplyMsg = #msg{}, NewMState} ->
         send_fix_message(ReplyMsg, Data),
         {ok, Data#data{module_state = NewMState, seq_num_out = Data#data.seq_num_out + 1}};
      {noreply, NewMState} ->
         {ok, Data#data{module_state = NewMState}};
      Reply ->
         {stop, {bad_return_value, Reply}, Data}
   end;

'LOGGED_IN'({send_fix, FixMsg}, Data) ->
   send_fix_message(FixMsg, Data),
   {ok, Data#data{seq_num_out = Data#data.seq_num_out + 1}}.

% ================= LOGGED_IN END =====================================================================

% =====================================================================================================
% =========================== Acceptor FSM end ========================================================
% =====================================================================================================

apply(Data = #data{session_id = SessionID, state = OldState}, Msg) ->
   case (catch erlang:apply(?MODULE, OldState, [Msg, Data])) of
      {ok, NewData = #data{state = NewState}} ->
         if NewState =/= OldState ->
               error_logger:info_msg("[~p]: state changed [~p]->[~p].", [SessionID, OldState, NewState]);
            true -> ok
         end,
         {ok, NewData};
      Reply = {stop, _Reason, _Data} ->
         Reply;
      {'EXIT', {function_clause, [_]}} ->
         error_logger:warning_msg("[~p]: unsupported state [~p] message [~p].", [SessionID, OldState, Msg]),
         {ok, Data};
      Other ->
         error_logger:error_msg("[~p]: wrong call [~p]", [SessionID, Other]),
         exit({error_wrong_result, Other})
   end.

parse_binary(<<>>, Data) ->
   {ok, Data};
parse_binary(Bin, Data = #data{binary = PrefixBin}) ->
   case fix_parser:binary_to_msg(Data#data.parser, ?FIX_SOH, <<PrefixBin/binary, Bin/binary>>) of
      {ok, Msg, RestBin} ->
         trace(Msg, in, Data),
         Data1 = Data#data{binary = <<>>},
         case ?MODULE:apply(Data1, Msg) of
            {ok, Data2} ->
               parse_binary(RestBin, Data2);
            Reply ->
               Reply
         end;
      {fix_error, ?FIX_ERROR_BODY_TOO_SHORT, _} ->
         {ok, Data#data{binary = <<PrefixBin/binary, Bin/binary>>}};
      {fix_error, ErrCode, ErrText} ->
         error_logger:error_msg("[~p]: unable to parse incoming message. Error = [~p], Description = [~p].",
            [Data#data.session_id, ErrCode, ErrText]),
         ?MODULE:apply(Data, {fix_error, ErrCode, ErrText})
   end.

restart_heartbeat(#data{timer_ref = undef, heartbeat_int = Timeout}) ->
   erlang:start_timer(Timeout, self(), heartbeat);
restart_heartbeat(#data{timer_ref = OldTimerRef, heartbeat_int = Timeout}) ->
   erlang:cancel_timer(OldTimerRef),
   erlang:start_timer(Timeout, self(), heartbeat).

validate_logon(Msg = #msg{type = "A"}, Username, Password) ->
   {ok, Username1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Username, ""),
   {ok, Password1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Password, ""),
   if ((Username == Username1) andalso (Password == Password1)) -> ok;
      true ->
         throw({error, "Wrong Username/Password"})
   end;
validate_logon(_, _, _) ->
   throw({error, "First message not a logon"}).

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
   error_logger:info_msg("[~p]: will use tracer.", [SessionID]);
print_use_tracer(_SessionID, _) ->
   ok.

socket_close(Socket) when is_port(Socket) ->
   gen_tcp:close(Socket);
socket_close(Socket) ->
   Socket ! tcp_close.

socket_send(Socket, Msg) when is_port(Socket) ->
   gen_tcp:send(Socket, Msg);
socket_send(Socket, Msg) ->
   Socket ! Msg,
   ok.

socket_controlling_process(Socket, SessionPid) when is_port(Socket) ->
   gen_tcp:controlling_process(Socket, SessionPid);
socket_controlling_process(_Socket, _SessionPid) ->
   ok.
