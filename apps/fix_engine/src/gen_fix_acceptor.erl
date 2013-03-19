-module(gen_fix_acceptor).

-include("fix_engine_config.hrl").
-include("fix_parser.hrl").
-include("fix_fields.hrl").

-behaviour(gen_server).

-export([set_socket/2, connect/1, disconnect/1, get_current_state/1, send_fix/2]).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([apply/2, 'DISCONNECTED'/2, 'CONNECTED'/2, 'LOGGED_IN'/2]).

-record(data, {
      module      :: atom(),
      module_state         :: term(),
      session_id           :: atom(),
      socket               :: port() | gen_tcp:socket(),
      tracer               :: pid(),
      storage              :: pid(),
      parser_in            :: #parser{},
      parser_out           :: #parser{},
      seq_num_out          :: pos_integer(),
      seq_num_in           :: pos_integer(),
      sender_comp_id       :: string(),
      target_comp_id       :: string(),
      username             :: string(),
      password             :: string(),
      state = 'DISCONNECTED' :: atom(),
      heartbeat_int        :: pos_integer(),
      timer_ref            :: reference(),
      binary = <<>>        :: binary()
   }).

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
-callback handle_resend(FIXMessage :: #msg{}, State :: term()) ->
   {true, NewState :: term()} | {false, NewState :: term()}.

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

start_link(Args = #fix_session_config{type = acceptor, session_id = SessionID}, Options) ->
   error_logger:info_msg("[~p]: starting.", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, Options).

init(#fix_session_config{session_id = SessionID, module = Module, module_args = MArgs, fix_protocol = Protocol,
      fix_parser_flags = ParserFlags, sender_comp_id = SenderCompID, target_comp_id = TargetCompID,
      username = Username, password = Password, tracer = Tracer, storage = Storage}) ->
   {ok, ParserInRef} = create_parser(SessionID, Protocol, [], ParserFlags),
   {ok, ParserOutRef} = create_parser(SessionID, Protocol, [], ParserFlags),
   Data = #data{module = Module,
      session_id = SessionID, parser_in = ParserInRef, parser_out = ParserOutRef,
      sender_comp_id = SenderCompID, target_comp_id = TargetCompID,
      username = Username, password = Password, tracer = Tracer, storage = Storage},
   {ok, Data1} = get_storage_stat(Data),
   error_logger:info_msg("[~p]: seq_num_in = ~p, seq_num_out = ~p.", [SessionID, Data1#data.seq_num_in, Data1#data.seq_num_out]),
   case Module:init(SessionID, ParserOutRef, MArgs) of
      {ok, State} ->
         {ok, Data1#data{module_state = State}};
      {ok, State, Timeout} ->
         {ok, Data1#data{module_state = State}, Timeout};
      {connect, State} ->
         {ok, Data2} = ?MODULE:apply(Data1, connect),
         {ok, Data2#data{module_state = State}};
      {connect, State, Timeout} ->
         {ok, Data2} = ?MODULE:apply(Data1, connect),
         {ok, Data2#data{module_state = State}, Timeout};
      Other ->
         Other
   end.

handle_call({gen_fix_acceptor, {set_socket, _Socket}}, _From, Data = #data{socket = OldSocket}) when is_port(OldSocket) ->
   {reply, {error, already_connected}, Data};
handle_call({gen_fix_acceptor, {set_socket, Socket}}, _From, Data) ->
   {reply, ok, Data#data{socket = Socket}};
handle_call({gen_fix_acceptor, connect}, _From, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, connect),
   {reply, ok, Data1};
handle_call({gen_fix_acceptor, disconnect}, _From, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, disconnect),
   {reply, ok, Data1};
handle_call({gen_fix_acceptor, get_current_state}, _From, Data) ->
   {reply, Data#data.state, Data};
handle_call({gen_fix_acceptor, send_fix, FixMsg}, _From, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, {send_fix, FixMsg}),
   {reply, ok, Data1};
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
      {ok, Data1 = #data{socket = S}} when is_port(S) ->
         inet:setopts(Socket, [{active, once}]),
         {noreply, Data1};
      {ok, Data1} ->
         {noreply, Data1};
      {stop, Reason, Data1} ->
         {stop, Reason, Data1}
   end;
handle_info({tcp_closed, Socket}, Data = #data{socket = Socket}) ->
   {ok, Data1} = ?MODULE:apply(Data, tcp_closed),
   {noreply, Data1};
handle_info({timeout, _, heartbeat}, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, heartbeat),
   {noreply, Data1};
handle_info({resend, Msgs}, Data) ->
   {ok, Data1} = resend(Msgs, Data),
   {noreply, Data1};
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

'DISCONNECTED'(heartbeat, Data) ->
   {ok, Data#data{timer_ref = undefined}};

'DISCONNECTED'(_, Data) ->
   error_logger:error_msg("[~p]: not connected. Do connect first.", [Data#data.session_id]),
   socket_close(Data).
% ================= DISCONNECTED END ==================================================================

% ================= CONNECTED BEGIN ===================================================================
'CONNECTED'(heartbeat, Data) ->
   {ok, Data#data{timer_ref = undefined}};

'CONNECTED'(disconnect, Data) ->
   {ok, Data#data{state = 'DISCONNECTED'}};

'CONNECTED'({fix_error, _ErrCode, ErrText}, Data) ->
   LogoutMsg = create_logout(Data#data.parser_out, ErrText),
   {ok, Data1} = send_fix_message(LogoutMsg, Data),
   socket_close(Data1);

'CONNECTED'(Msg, Data = #data{session_id = SessionID, state = 'CONNECTED'}) ->
   error_logger:info_msg("[~p]: message [~p] received.", [Data#data.session_id, Msg#msg.type]),
   try
      validate_logon(Msg, Data#data.username, Data#data.password),
      {ok, HeartBtInt} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_HeartBtInt),
      {ok, ResetSeqNum} = fix_parser:get_char_field(Msg, ?FIXFieldTag_ResetSeqNumFlag, $N),
      {ok, SeqNum} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_MsgSeqNum),
      {ok, Data1} = if ResetSeqNum == $Y -> reset_storage(Data); true -> {ok, Data} end,
      Data2 = Data1#data{heartbeat_int = HeartBtInt},
      LogonReply = create_logon(ResetSeqNum, Data2),
      {ok, Data3} = send_fix_message(LogonReply, Data2),
      {ok, Data4} = restart_heartbeat(Data3),
      {ok, Data5} = store_seq_num_in(SeqNum, Data4),
      {ok, Data5#data{state = 'LOGGED_IN'}}
   catch
      throw:{badmatch, {fix_error, _, ErrText}} ->
         error_logger:error_msg("[~p]: logon failed: ~p", [SessionID, ErrText]),
         LogoutMsg = create_logout(Data#data.parser_out, ErrText),
         {ok, Data6} = send_fix_message(LogoutMsg, Data),
         {ok, Data7} = socket_close(Data6),
         {ok, Data7#data{state = 'CONNECTED'}};
      throw:{error, Reason} ->
         error_logger:error_msg("[~p]: logon failed: ~p", [SessionID, Reason]),
         LogoutMsg = create_logout(Data#data.parser_out, Reason),
         {ok, Data6} = send_fix_message(LogoutMsg, Data),
         {ok, Data7} = socket_close(Data6),
         {ok, Data7#data{state = 'CONNECTED'}};
      _:Err ->
         error_logger:error_msg("[~p]: logon failed: ~p", [SessionID, Err]),
         LogoutMsg = create_logout(Data#data.parser_out, "Logon failed"),
         {ok, Data6} = send_fix_message(LogoutMsg, Data),
         {ok, Data7} = socket_close(Data6),
         {ok, Data7#data{state = 'CONNECTED'}}
   end.

% ================= CONNECTED END =====================================================================

% ================= LOGGED_IN BEGIN ===================================================================
'LOGGED_IN'(heartbeat, Data) ->
   {ok, Msg} = fix_parser:create_msg(Data#data.parser_out, "0"),
   {ok, Data1} = send_fix_message(Msg, Data),
   restart_heartbeat(Data1);

'LOGGED_IN'(disconnect, Data) ->
   Msg = create_logout(Data#data.parser_out, "Explicitly disconnected"),
   {ok, Data1} = send_fix_message(Msg, Data),
   {ok, Data2} = socket_close(Data1),
   {ok, Data3} = cancel_heartbeat(Data2),
   {ok, Data3#data{state = 'DISCONNECTED'}};

'LOGGED_IN'(tcp_closed, Data) ->
   {ok, Data1} = cancel_heartbeat(Data),
   {ok, Data1#data{socket = undefined, state = 'CONNECTED'}};

'LOGGED_IN'(#msg{type = "A"}, Data) ->
   error_logger:error_msg("[~p]: unexpected logon received.", [Data#data.session_id]),
   {ok, Data1} = socket_close(Data),
   {ok, Data2} = cancel_heartbeat(Data1),
   {ok, Data2#data{state = 'CONNECTED'}};

'LOGGED_IN'(HeartbeatMsg = #msg{type = "0"}, Data) ->
   {ok, SeqNum} = fix_parser:get_int32_field(HeartbeatMsg, ?FIXFieldTag_MsgSeqNum),
   store_seq_num_in(SeqNum, Data);

'LOGGED_IN'(LogoutMsg = #msg{type = "5"}, Data) ->
   Logout = create_logout(Data#data.parser_out, "Bye"),
   {ok, Data1} = send_fix_message(Logout, Data),
   {ok, SeqNum} = fix_parser:get_int32_field(LogoutMsg, ?FIXFieldTag_MsgSeqNum),
   {ok, Data2} = cancel_heartbeat(Data1),
   store_seq_num_in(SeqNum, Data2);

'LOGGED_IN'(TestRequestMsg = #msg{type = "1"}, Data) ->
   {ok, TestReqID} = fix_parser:get_string_field(TestRequestMsg, ?FIXFieldTag_TestReqID),
   {ok, HeartbeatMsg} = fix_parser:create_msg(Data#data.parser_out, "0"),
   ok = fix_parser:set_string_field(HeartbeatMsg, ?FIXFieldTag_TestReqID, TestReqID),
   {ok, Data1} = send_fix_message(HeartbeatMsg, Data),
   {ok, Data2} = restart_heartbeat(Data1),
   {ok, SeqNum} = fix_parser:get_int32_field(TestRequestMsg, ?FIXFieldTag_MsgSeqNum),
   store_seq_num_in(SeqNum, Data2);

'LOGGED_IN'(ResendRequestMsg = #msg{type = "2"}, Data = #data{storage = Storage}) ->
   {ok, BeginSeqNo} = fix_parser:get_int32_field(ResendRequestMsg, ?FIXFieldTag_BeginSeqNo),
   {ok, EndSeqNo} = fix_parser:get_int32_field(ResendRequestMsg, ?FIXFieldTag_EndSeqNo),
   fix_storage:resend(Storage, BeginSeqNo, EndSeqNo),
   {ok, Data};

'LOGGED_IN'(Msg = #msg{}, Data = #data{module = Module, module_state = MState}) ->
   {ok, SeqNum} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_MsgSeqNum),
   {ok, Data1} = store_seq_num_in(SeqNum, Data),
   case Module:handle_fix(Msg, MState) of
      {reply, ReplyMsg = #msg{}, NewMState} ->
         {ok, Data2} = send_fix_message(ReplyMsg, Data),
         {ok, Data3} = restart_heartbeat(Data2),
         {ok, Data3#data{module_state = NewMState}};
      {noreply, NewMState} ->
         {ok, Data1#data{module_state = NewMState}};
      Reply ->
         {stop, {bad_return_value, Reply}, Data1}
   end;

'LOGGED_IN'({send_fix, FixMsg}, Data) ->
   {ok, Data1} = send_fix_message(FixMsg, Data),
   restart_heartbeat(Data1);

'LOGGED_IN'({skip_resend, SkipFrom, SkipTo}, Data = #data{session_id = SessionID}) ->
   error_logger:info_msg("[~p]: [~p, ~p] message(s) will be skipped.", [SessionID, SkipFrom, SkipTo]),
   %TODO: do skip here
   {ok, Data};

'LOGGED_IN'({resend, FixMsg}, Data = #data{session_id = SessionID}) ->
   {ok, MsgSeqNum} = fix_parser:get_int32_field(FixMsg, ?FIXFieldTag_MsgSeqNum),
   error_logger:info_msg("[~p]: ~s message with MsgSeqNum = ~p will be resent.", [SessionID, FixMsg#msg.type, MsgSeqNum]),
   %TODO: do resend here
   {ok, Data};

'LOGGED_IN'({fix_error, ErrCode, ErrText}, Data = #data{session_id = SessionID}) ->
   error_logger:error_msg("[~p]: Error = ~p. Description = ~p.", [SessionID, ErrCode, ErrText]),
   % TODO: send levelReject here
   {ok, Data}.

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
   case fix_parser:binary_to_msg(Data#data.parser_in, ?FIX_SOH, <<PrefixBin/binary, Bin/binary>>) of
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

cancel_heartbeat(Data = #data{timer_ref = undefined}) ->
   {ok, Data};
cancel_heartbeat(Data = #data{timer_ref = TimerRef}) ->
   erlang:cancel_timer(TimerRef),
   {ok, Data#data{timer_ref = undefined}}.

restart_heartbeat(Data = #data{timer_ref = undefined, heartbeat_int = Timeout}) ->
   TimerRef = erlang:start_timer(Timeout * 1000, self(), heartbeat),
   {ok, Data#data{timer_ref = TimerRef}};
restart_heartbeat(Data = #data{timer_ref = OldTimerRef, heartbeat_int = Timeout}) ->
   erlang:cancel_timer(OldTimerRef),
   TimerRef = erlang:start_timer(Timeout * 1000, self(), heartbeat),
   {ok, Data#data{timer_ref = TimerRef}}.

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

create_logon(ResetSeqNum, #data{parser_out = Parser, heartbeat_int = HeartBtInt}) ->
   {ok, Msg} = fix_parser:create_msg(Parser, "A"),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_HeartBtInt, HeartBtInt),
   ok = fix_parser:set_char_field(Msg, ?FIXFieldTag_ResetSeqNumFlag, ResetSeqNum),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_EncryptMethod, 0),
   Msg.

send_fix_message(Msg, Data = #data{seq_num_out = SeqNumOut}) ->
   NewSeqNumOut = SeqNumOut + 1,
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SenderCompID, Data#data.sender_comp_id),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_TargetCompID, Data#data.target_comp_id),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_MsgSeqNum, NewSeqNumOut),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SendingTime, fix_utils:now_utc()),
   {ok, BinMsg} = fix_parser:msg_to_binary(Msg, ?FIX_SOH),
   ok = socket_send(Data#data.socket, BinMsg),
   trace(Msg, out, Data),
   store_msg_out(NewSeqNumOut, Msg#msg.type, BinMsg, Data).

trace(_, _, #data{tracer = undefined}) -> ok;
trace(Msg, Direction, #data{tracer = Tracer}) ->
   fix_tracer:trace(Tracer, Direction, Msg).

store_msg_out(SeqNumOut, _Type, _, Data = #data{storage = undefined}) ->
   {ok, Data#data{seq_num_out = SeqNumOut}};
store_msg_out(SeqNumOut, Type, Msg, Data = #data{storage = Storage}) ->
   fix_storage:store_msg_out(Storage, SeqNumOut, Type, Msg),
   {ok, Data#data{seq_num_out = SeqNumOut}}.

store_seq_num_in(SeqNumIn, Data = #data{storage = undefined}) ->
   {ok, Data#data{seq_num_in = SeqNumIn}};
store_seq_num_in(SeqNumIn, Data = #data{storage = Storage}) ->
   fix_storage:store_seq_num_in(Storage, SeqNumIn),
   {ok, Data#data{seq_num_in = SeqNumIn}}.

get_storage_stat(Data = #data{storage = undefined}) ->
   {ok, Data#data{seq_num_in = 0, seq_num_out = 0}};
get_storage_stat(Data = #data{storage = Storage}) ->
   {ok, {SeqNumIn, SeqNumOut}} = fix_storage:get_stat(Storage),
   {ok, Data#data{seq_num_in = SeqNumIn, seq_num_out = SeqNumOut}}.

reset_storage(Data = #data{storage = undefined}) ->
   {ok, Data#data{seq_num_in = 0, seq_num_out = 0}};
reset_storage(Data = #data{storage = Storage}) ->
   fix_storage:reset(Storage),
   {ok, Data#data{seq_num_in = 0, seq_num_out = 0}}.

socket_close(Data = #data{socket = Socket}) when is_port(Socket) ->
   gen_tcp:close(Socket),
   {ok, Data#data{socket = undefined}};
socket_close(Data = #data{socket = Socket}) ->
   Socket ! tcp_close,
   {ok, Data}.

socket_send(Socket, Msg) when is_port(Socket) ->
   gen_tcp:send(Socket, Msg);
socket_send(Socket, Msg) ->
   Socket ! Msg,
   ok.

socket_controlling_process(Socket, SessionPid) when is_port(Socket) ->
   gen_tcp:controlling_process(Socket, SessionPid);
socket_controlling_process(_Socket, _SessionPid) ->
   ok.

create_parser(SessionID, Protocol, Attributes, ParserFlags) ->
   case fix_parser:create(Protocol, Attributes, ParserFlags) of
      {ok, ParserRef} ->
         error_logger:info_msg("[~p]: parser [~p] has been created.", [SessionID, fix_parser:get_version(ParserRef)]);
      {fix_error, ParserRef} ->
         exit({fix_parser_error, ParserRef})
   end,
   {ok, ParserRef}.

resend(Msgs, Data) ->
   resend(Msgs, {0, 0}, Data).

resend([], {0, 0}, Data) ->
   {ok, Data};

resend([], {SkipFrom, SkipTo}, Data) ->
   ?MODULE:apply(Data, {skip_resend, SkipFrom, SkipTo});

resend([{SeqNum, Type, _BinMsg}|Rest], {0, 0}, Data)
      when Type == "0" orelse Type == "A" orelse Type == "5" orelse
           Type == "3" orelse Type == "2" orelse Type == "4" orelse Type == "1" ->
   resend(Rest, {SeqNum, SeqNum}, Data);

resend([{SeqNum, Type, _BinMsg}|Rest], {SkipFrom, _}, Data)
      when Type == "0" orelse Type == "A" orelse Type == "5" orelse
           Type == "3" orelse Type == "2" orelse Type == "4" orelse Type == "1" ->
   resend(Rest, {SkipFrom, SeqNum}, Data);

resend([{SeqNum, _Type, BinMsg}|Rest], {0, 0}, Data = #data{module = Module, module_state = MState}) ->
   {ok, Msg, <<>>} = fix_parser:binary_to_msg(Data#data.parser_out, ?FIX_SOH, BinMsg),
   case Module:handle_resend(Msg, MState) of
      {true, MState1} ->
         {ok, Data1} = ?MODULE:apply(Data, {resend, Msg}),
         SkipFrom = SkipTo = 0;
      {false, MState1} ->
         Data1 = Data,
         SkipFrom = SkipTo = SeqNum
   end,
   resend(Rest, {SkipFrom, SkipTo}, Data1#data{module_state = MState1});

resend([{SeqNum, _Type, BinMsg}|Rest], {SkipFrom, SkipTo}, Data = #data{module = Module, module_state = MState}) ->
   {ok, Msg, <<>>} = fix_parser:binary_to_msg(Data#data.parser_out, ?FIX_SOH, BinMsg),
   case Module:handle_resend(Msg, MState) of
      {true, MState1} ->
         {ok, Data1} = ?MODULE:apply(Data, {skip_resend, SkipFrom, SkipTo}),
         {ok, Data2} = ?MODULE:apply(Data1, {resend, Msg});
      {false, MState1} ->
         {ok, Data2} = ?MODULE:apply(Data, {skip_resend, SkipFrom, SeqNum})
   end,
   resend(Rest, {0, 0}, Data2#data{module_state = MState1}).
