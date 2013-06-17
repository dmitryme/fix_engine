-module(gen_fix_session).

-include("fix_engine_config.hrl").
-include("fix_parser.hrl").
-include("fix_fields.hrl").

-behaviour(gen_server).

-export([set_socket/2, connect/1, disconnect/1, get_current_state/1, send_fix/2]).

-export([start_link/3, start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([apply/2, 'DISCONNECTED'/2, 'CONNECTED'/2, 'LOGGED_IN'/2]).

-record(data, {
      module_state               :: term(),
      socket                     :: port() | gen_tcp:socket(),
      parser_in                  :: #parser{},
      parser_out                 :: #parser{},
      seq_num_out                :: pos_integer(),
      seq_num_in                 :: pos_integer(),
      state = 'DISCONNECTED'     :: atom(),
      heartbeat_timer_ref        :: reference(),
      timeout_timer_ref          :: reference(),
      binary              = <<>> :: binary(),
      reconnect_tries     = 0    :: pos_integer(),
      config                     :: #fix_session_config{}
   }).

-callback init(atom(), FixState :: atom(), #parser{}, term()) ->
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

-callback handle_fix_state(NewFixState :: atom(), OldFixState :: atom(), State :: term()) ->
   {ok, NewState :: term()}.

-callback handle_resend(FIXMessage :: #msg{}, State :: term()) ->
   {true, NewState :: term()} | {false, NewState :: term()}.

-callback handle_error({Error :: atom(), Reason :: term()}, FIXMessage :: #msg{}, State :: term()) ->
   {ok, NewState :: term()}.

set_socket(SessionPid, Socket) ->
   case gen_server:call(SessionPid, {gen_fix_session, {set_socket, Socket}}) of
      Err = {error, _Reason} ->
         Err;
      ok ->
         socket_controlling_process(Socket, SessionPid),
         ok
   end.

connect(Name) ->
   gen_server:call(Name, {gen_fix_session, connect}).

disconnect(Name) ->
   gen_server:call(Name, {gen_fix_session, disconnect}).

get_current_state(Name) ->
   gen_server:call(Name, {gen_fix_session, get_current_state}).

send_fix(Name, FixMsg) ->
   gen_server:cast(Name, {gen_fix_session, send_fix, FixMsg}).

start_link(Name, Args = #fix_session_config{session_id = SessionID}, Options) ->
   error_logger:info_msg("[~p]: starting.", [SessionID]),
   gen_server:start_link(Name, ?MODULE, Args, Options).

start_link(Args = #fix_session_config{session_id = SessionID}, Options) ->
   error_logger:info_msg("[~p]: starting.", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, Options).

init(Config = #fix_session_config{session_id = SessionID, module = Module,
      module_args = MArgs, fix_protocol = Protocol, fix_parser_flags = ParserFlags}) ->
   {ok, ParserInRef} = create_parser(SessionID, Protocol, [], ParserFlags),
   {ok, ParserOutRef} = create_parser(SessionID, Protocol, [], ParserFlags),
   Data = #data{parser_in = ParserInRef, parser_out = ParserOutRef, config = Config},
   {ok, Data1} = get_storage_stat(Data),
   error_logger:info_msg("[~p]: seq_num_in = ~p, seq_num_out = ~p.", [SessionID, Data1#data.seq_num_in, Data1#data.seq_num_out]),
   case Module:init(SessionID, Data#data.state, ParserOutRef, MArgs) of
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

handle_call({gen_fix_session, {set_socket, _Socket}}, _From, Data = #data{socket = OldSocket}) when is_port(OldSocket) orelse is_pid(OldSocket) ->
   {reply, {error, already_connected}, Data};
handle_call({gen_fix_session, {set_socket, Socket}}, _From, Data) ->
   {reply, ok, Data#data{socket = Socket}};
handle_call({gen_fix_session, connect}, _From, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, connect),
   {reply, ok, Data1};
handle_call({gen_fix_session, disconnect}, _From, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, disconnect),
   {reply, ok, Data1};
handle_call({gen_fix_session, get_current_state}, _From, Data) ->
   {reply, Data#data.state, Data};
handle_call(Msg, From, Data = #data{config = #fix_session_config{module = Module}, module_state = MState}) ->
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

handle_cast({gen_fix_session, send_fix, FixMsg}, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, {send_fix, FixMsg}),
   {noreply, Data1};
handle_cast(Request, Data = #data{config = #fix_session_config{module = Module}, module_state = MState}) ->
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

handle_info({timeout, _, Type}, Data) ->
   {ok, Data1} = ?MODULE:apply(Data, Type),
   {noreply, Data1};

handle_info({resend, Msgs}, Data) ->
   {ok, Data1} = resend(Msgs, Data),
   {noreply, Data1};

handle_info(Info, Data = #data{config = #fix_session_config{module = Module}, module_state = MState}) ->
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

terminate(Reason, #data{config = #fix_session_config{module = Module}, module_state = MState}) ->
   Module:terminate(Reason, MState).

code_change(OldVsn, Data  = #data{config = #fix_session_config{module = Module}, module_state = MState}, Extra) ->
   case Module:code_change(OldVsn, MState, Extra) of
      {ok, NewMState} ->
         {ok, Data#data{module_state = NewMState}};
      Reply ->
         Reply
   end.

% =====================================================================================================
% ==================================== FSM begin ======================================================
% =====================================================================================================

% ================= DISCONNECTED BEGIN ================================================================
'DISCONNECTED'(connect, Data = #data{config = #fix_session_config{type = acceptor}}) ->
   {ok, Data#data{state = 'CONNECTED'}};

'DISCONNECTED'(connect, Data = #data{reconnect_tries = ReconnectTries, config = #fix_session_config{
      type = initiator, session_id = SessionID, host = Host, port = Port,
      reconnect_int = ReconnectInt, reconnect_max_tries = ReconnectMaxTries, socket_opts = SocketOpts,
      logon_timeout = LogonTimeout, reset_seq_num = ResetSeqNumFlag, storage = Storage}}) ->
      case gen_tcp:connect(Host, Port, [{active, once}, binary | SocketOpts]) of
      {ok, Socket} ->
         Data1 = Data#data{state = 'CONNECTED', socket = Socket, reconnect_tries = 0},
         {ok, CorrectlyTerminated} = fix_storage:get_metadata(Storage, correctly_terminated),
         {ok, SeqNumOut} = fix_storage:get_metadata(Storage, seq_num_out),
         ResetSeqNum = if (ResetSeqNumFlag == always) orelse
                          (ResetSeqNumFlag == logout andalso CorrectlyTerminated == true) orelse
                          (ResetSeqNumFlag == failure andalso CorrectlyTerminated == true) orelse
                          (ResetSeqNumFlag == never andalso SeqNumOut == 0) ->
               {ok, Data2} = reset_storage(Data1),
               $Y;
         true ->
               Data2 = Data1,
               $N
         end,
         {ok, LogonMsg} = create_logon(ResetSeqNum, Data2),
         {ok, Data3} = send_fix_message([LogonMsg], Data2),
         {ok, DataRes} = restart_timeout_timer(logon_timeout, LogonTimeout * 1000, Data3);
      {error, Reason} when ReconnectTries =< ReconnectMaxTries orelse ReconnectMaxTries == 0 ->
         error_logger:error_msg("[~p]: unable to connect to ~s:~p. Reason = ~p.", [SessionID, Host, Port, Reason]),
         {ok, Data1} = restart_timeout_timer(connect, ReconnectInt, Data),
         DataRes = Data1#data{reconnect_tries = Data1#data.reconnect_tries + 1};
      {error, Reason} when ReconnectTries > ReconnectMaxTries ->
         error_logger:error_msg("[~p]: unable to connect to ~s:~p. Reason = ~p.", [SessionID, Host, Port, Reason]),
         error_logger:error_msg("[~p]: reconnect tries count exceeded limit ~p. Reconnection stopped.", [SessionID, ReconnectMaxTries]),
         DataRes = Data#data{reconnect_tries = 0}
   end,
   {ok, DataRes};

'DISCONNECTED'(heartbeat, Data) ->
   {ok, Data#data{heartbeat_timer_ref = undefined}};

'DISCONNECTED'(timeout, Data) ->
   {ok, Data#data{heartbeat_timer_ref = undefined}};

'DISCONNECTED'(Msg, Data) ->
   error_logger:error_msg("[~p]: unsupported msg ~p received.", [Data#data.config#fix_session_config.session_id, Msg]),
   socket_close(Data).
% ================= DISCONNECTED END ==================================================================

% ================= CONNECTED BEGIN ===================================================================
'CONNECTED'(heartbeat, Data) ->
   {ok, Data#data{heartbeat_timer_ref = undefined}};

'CONNECTED'(disconnect, Data) ->
   {ok, Data1} = cancel_timeout_timer(Data),
   {ok, Data1#data{state = 'DISCONNECTED'}};

'CONNECTED'({fix_error, _ErrCode, ErrText}, Data) ->
   {ok, LogoutMsg} = create_logout(ErrText, Data),
   {ok, Data1} = send_fix_message([LogoutMsg], Data),
   socket_close(Data1);

'CONNECTED'(logon_timeout, Data = #data{config = #fix_session_config{type = initiator, session_id = SessionID, reconnect_int = ReconnectInt}}) ->
   error_logger:error_msg("[~p]: no logon response within timeout interval.", [SessionID]),
   {ok, Data1} = restart_timeout_timer(connect, ReconnectInt, Data),
   {ok, Data2} = socket_close(Data1),
   {ok, Data2#data{state = 'DISCONNECTED'}};

'CONNECTED'(tcp_closed, Data = #data{config = #fix_session_config{type = initiator, session_id = SessionID, reconnect_int = ReconnectInt}}) ->
   error_logger:error_msg("[~p]: socket closed by peer.", [SessionID]),
   {ok, Data1} = cancel_timeout_timer(Data),
   {ok, Data2} = restart_timeout_timer(connect, ReconnectInt, Data1),
   {ok, Data2#data{state = 'DISCONNECTED'}};

'CONNECTED'(Msg = #msg{type = "A"}, Data = #data{config = #fix_session_config{type = initiator, session_id = SessionID}}) ->
   {ok, MsgSeqNum} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_MsgSeqNum),
   {ok, Data1} = store_seq_num_in(MsgSeqNum, Data),
   {ok, Data2} = restart_heartbeat_timer(Data1),
   {ok, Data3} = cancel_timeout_timer(Data2),
   error_logger:info_msg("[~p]: successfully logged in.", [SessionID]),
   {ok, Data3#data{state = 'LOGGED_IN'}};

'CONNECTED'(Msg = #msg{type = "A"}, Data = #data{config = #fix_session_config{storage = Storage, type = acceptor, session_id = SessionID},
      seq_num_in = SeqNumIn}) ->
   error_logger:info_msg("[~p]: message [~p] received.", [Data#data.config#fix_session_config.session_id, Msg#msg.type]),
   try
      validate_logon(Msg, Data#data.config#fix_session_config.username, Data#data.config#fix_session_config.password),
      {ok, HeartBtInt} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_HeartBtInt),
      {ok, ResetSeqNum} = fix_parser:get_char_field(Msg, ?FIXFieldTag_ResetSeqNumFlag, $N),
      {ok, MsgSeqNum} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_MsgSeqNum),
      {ok, PossDupFlag} = fix_parser:get_char_field(Msg, ?FIXFieldTag_PossDupFlag, $N),
      Data1 = Data#data{config = Data#data.config#fix_session_config{heartbeat_int = HeartBtInt}},
      case ResetSeqNum == $Y of
         true ->
            {ok, Data2} = reset_storage(Data1),
            {ok, LogonReply} = create_logon(ResetSeqNum, Data2),
            {ok, Data3} = send_fix_message([LogonReply], Data2),
            {ok, Data4} = restart_heartbeat_timer(Data3),
            {ok, DataRes} = store_seq_num_in(MsgSeqNum, Data4);
         false when MsgSeqNum =< SeqNumIn andalso PossDupFlag == $N ->
            DataRes = Data1,
            throw({error, io_lib:format("MsgSeqNum too low, expecting ~p but received ~p", [SeqNumIn + 1, MsgSeqNum])});
         false when MsgSeqNum =:= SeqNumIn + 1 ->
            {ok, LogonReply} = create_logon(ResetSeqNum, Data1),
            {ok, Data2} = send_fix_message([LogonReply], Data1),
            {ok, Data3} = restart_heartbeat_timer(Data2),
            {ok, DataRes} = store_seq_num_in(MsgSeqNum, Data3);
         false when MsgSeqNum > SeqNumIn + 1 ->
            {ok, LogonReply} = create_logon(ResetSeqNum, Data1),
            {ok, ResendRequest} = create_resend(SeqNumIn + 1, 0, Data1),
            {ok, Data2} = send_fix_message([LogonReply, ResendRequest], Data1),
            {ok, DataRes} = restart_heartbeat_timer(Data2)
      end,
      ok = fix_storage:set_metadata(Storage, correctly_terminated, false),
      {ok, DataRes#data{state = 'LOGGED_IN'}}
   catch
      throw:{badmatch, {fix_error, _, Reason}} ->
         error_logger:error_msg("[~p]: logon failed: ~p", [SessionID, lists:flatten(Reason)]),
         {ok, Data100} = reject_logon(Reason, Data),
         {ok, Data100#data{state = 'CONNECTED'}};
      throw:{error, Reason} ->
         error_logger:error_msg("[~p]: logon failed: ~p", [SessionID, lists:flatten(Reason)]),
         {ok, Data100} = reject_logon(Reason, Data),
         {ok, Data100#data{state = 'CONNECTED'}};
      _:Err ->
         error_logger:error_msg("[~p]: logon failed: ~p", [SessionID, Err]),
         {ok, Data100} = reject_logon("Logon failed.", Data),
         {ok, Data100#data{state = 'CONNECTED'}}
   end;

'CONNECTED'(#msg{type = Type}, Data = #data{config = #fix_session_config{type = initiator, session_id = SessionID, reconnect_int = ReconnectInt}}) ->
   {ok, Data1} = restart_timeout_timer(connect, ReconnectInt, Data),
   error_logger:info_msg("[~p]: first message '~s' not a logon", [SessionID, Type]),
   {ok, Data2} = socket_close(Data1),
   {ok, Data2#data{state = 'DISCONNECTED'}}.

% ================= CONNECTED END =====================================================================

% ================= LOGGED_IN BEGIN ===================================================================
'LOGGED_IN'(heartbeat, Data) ->
   {ok, Msg} = fix_parser:create_msg(Data#data.parser_out, "0"),
   {ok, Data1} = send_fix_message([Msg], Data),
   restart_heartbeat_timer(Data1);

'LOGGED_IN'(timeout, Data = #data{config = #fix_session_config{session_id = SessionID, heartbeat_int = Timeout, transmission_time = TransTime}}) ->
   error_logger:info_msg("[~p]: timeout detected.", [SessionID]),
   {ok, Msg} = fix_parser:create_msg(Data#data.parser_out, "1"),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_TestReqID, erlang:ref_to_list(make_ref())),
   {ok, Data1} = send_fix_message([Msg], Data),
   {ok, Data2} = restart_heartbeat_timer(Data1),
   restart_timeout_timer(test_request_timeout, Timeout * 1000 + TransTime, Data2);

'LOGGED_IN'(test_request_timeout, Data = #data{config = #fix_session_config{session_id = SessionID}}) ->
   error_logger:error_msg("[~p]: connection considered lost.", [SessionID]),
   {ok, Data1} = socket_close(Data),
   {ok, Data2} = cancel_heartbeat_timer(Data1),
   {ok, Data3} = cancel_timeout_timer(Data2),
   {ok, Data3#data{state = 'CONNECTED', socket = undefined, binary = <<>>}};

'LOGGED_IN'(disconnect, Data) ->
   {ok, Msg} = create_logout("Explicitly disconnected", Data),
   {ok, Data1} = send_fix_message([Msg], Data),
   {ok, Data2} = socket_close(Data1),
   {ok, Data3} = cancel_heartbeat_timer(Data2),
   {ok, Data4} = cancel_timeout_timer(Data3),
   {ok, Data4#data{state = 'DISCONNECTED'}};

'LOGGED_IN'(tcp_closed, Data = #data{config = #fix_session_config{storage = Storage}}) ->
   {ok, Data1} = cancel_heartbeat_timer(Data),
   {ok, Data2} = cancel_timeout_timer(Data1),
   ok = fix_storage:get_metadata(Storage, correctly_terminated, true),
   {ok, Data2#data{state = 'CONNECTED', socket = undefined, binary = <<>>}};

'LOGGED_IN'(#msg{type = "A"}, Data = #data{config = #fix_session_config{session_id = SessionID}}) ->
   error_logger:error_msg("[~p]: unexpected logon received.", [SessionID]),
   {ok, Data1} = socket_close(Data),
   {ok, Data2} = cancel_heartbeat_timer(Data1),
   {ok, Data3} = cancel_timeout_timer(Data2),
   {ok, Data3#data{state = 'CONNECTED'}};

'LOGGED_IN'(HeartbeatMsg = #msg{type = "0"}, Data) ->
   F = fun() ->
      {ok, MsgSeqNum} = fix_parser:get_int32_field(HeartbeatMsg, ?FIXFieldTag_MsgSeqNum),
      store_seq_num_in(MsgSeqNum, Data)
   end,
   check_gap(HeartbeatMsg, F, Data);

'LOGGED_IN'(LogoutMsg = #msg{type = "5"}, Data = #data{config = #fix_session_config{storage = Storage}}) ->
   F = fun() ->
      {ok, MsgSeqNum} = fix_parser:get_int32_field(LogoutMsg, ?FIXFieldTag_MsgSeqNum),
      {ok, Logout} = create_logout("Bye", Data),
      {ok, Data1} = send_fix_message([Logout], Data),
      {ok, Data2} = cancel_heartbeat_timer(Data1),
      {ok, Data3} = cancel_timeout_timer(Data2),
      ok = fix_storage:get_metadata(Storage, correctly_terminated, true),
      store_seq_num_in(MsgSeqNum, Data3)
   end,
   check_gap(LogoutMsg, F, Data);

'LOGGED_IN'(TestRequestMsg = #msg{type = "1"}, Data) ->
   F = fun() ->
      {ok, MsgSeqNum} = fix_parser:get_int32_field(TestRequestMsg, ?FIXFieldTag_MsgSeqNum),
      {ok, TestReqID} = fix_parser:get_string_field(TestRequestMsg, ?FIXFieldTag_TestReqID),
      {ok, HeartbeatMsg} = fix_parser:create_msg(Data#data.parser_out, "0"),
      ok = fix_parser:set_string_field(HeartbeatMsg, ?FIXFieldTag_TestReqID, TestReqID),
      {ok, Data1} = send_fix_message([HeartbeatMsg], Data),
      {ok, Data2} = restart_heartbeat_timer(Data1),
      store_seq_num_in(MsgSeqNum, Data2)
   end,
   check_gap(TestRequestMsg, F, Data);

'LOGGED_IN'(ResendRequestMsg = #msg{type = "2"}, Data = #data{config = #fix_session_config{storage = Storage}}) ->
   {ok, MsgSeqNum} = fix_parser:get_int32_field(ResendRequestMsg, ?FIXFieldTag_MsgSeqNum),
   {ok, BeginSeqNo} = fix_parser:get_int32_field(ResendRequestMsg, ?FIXFieldTag_BeginSeqNo),
   {ok, EndSeqNo} = fix_parser:get_int32_field(ResendRequestMsg, ?FIXFieldTag_EndSeqNo),
   fix_storage:get_messages(Storage, BeginSeqNo, EndSeqNo),
   F = fun() ->
      store_seq_num_in(MsgSeqNum, Data)
   end,
   check_gap(ResendRequestMsg, F, Data);

'LOGGED_IN'(SequenceReset = #msg{type = "4"}, Data = #data{config = #fix_session_config{session_id = SessionID}}) ->
   {ok, GapFillFlag} = fix_parser:get_char_field(SequenceReset, ?FIXFieldTag_GapFillFlag, $N),
   {ok, NewSeqNo} = fix_parser:get_int32_field(SequenceReset, ?FIXFieldTag_NewSeqNo),
   case GapFillFlag of
      $N ->
         error_logger:info_msg("[~p]: SeqNumIn has been reset to ~p.", [SessionID, NewSeqNo]),
         store_seq_num_in(NewSeqNo, Data);
      $Y ->
         F = fun() ->
            {ok, MsgSeqNum} = fix_parser:get_int32_field(SequenceReset, ?FIXFieldTag_MsgSeqNum),
            error_logger:info_msg("[~p]: gap [~p,~p] filled.", [SessionID, MsgSeqNum, NewSeqNo - 1]),
            store_seq_num_in(NewSeqNo - 1, Data)
         end,
         check_gap(SequenceReset, F, Data)
   end;

'LOGGED_IN'(Msg = #msg{}, Data = #data{config = #fix_session_config{module = Module}, module_state = MState}) ->
   F = fun() ->
      {ok, MsgSeqNum} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_MsgSeqNum),
      case Module:handle_fix(Msg, MState) of
         {reply, ReplyMsg = #msg{}, NewMState} ->
            {ok, Data1} = send_fix_message([ReplyMsg], Data),
            {ok, Data2} = restart_heartbeat_timer(Data1),
            {ok, Data3} = store_seq_num_in(MsgSeqNum, Data2),
            {ok, Data3#data{module_state = NewMState}};
         {noreply, NewMState} ->
            {ok, Data1} = store_seq_num_in(MsgSeqNum, Data),
            {ok, Data1#data{module_state = NewMState}};
         Reply ->
            {stop, {bad_return_value, Reply}, Data}
      end
   end,
   check_gap(Msg, F, Data);

'LOGGED_IN'({send_fix, FixMsg}, Data) ->
   {ok, Data1} = send_fix_message([FixMsg], Data),
   restart_heartbeat_timer(Data1);

'LOGGED_IN'({skip_resend, SkipFrom, SkipTo}, Data = #data{parser_out = Parser, config = #fix_session_config{session_id = SessionID}}) ->
   error_logger:info_msg("[~p]: [~p, ~p] message(s) will be skipped.", [SessionID, SkipFrom, SkipTo]),
   {ok, SeqReset} = fix_parser:create_msg(Parser, "4"),
   ok = fix_parser:set_char_field(SeqReset, ?FIXFieldTag_PossDupFlag, $Y),
   ok = fix_parser:set_char_field(SeqReset, ?FIXFieldTag_GapFillFlag, $Y),
   ok = fix_parser:set_int32_field(SeqReset, ?FIXFieldTag_NewSeqNo, SkipTo + 1),
   {ok, Data1} = resend_fix_message(SkipFrom, SeqReset, Data),
   restart_heartbeat_timer(Data1);

'LOGGED_IN'({resend, FixMsg}, Data = #data{config = #fix_session_config{session_id = SessionID}}) ->
   {ok, MsgSeqNum} = fix_parser:get_int32_field(FixMsg, ?FIXFieldTag_MsgSeqNum),
   {ok, SendingTime} = fix_parser:get_string_field(FixMsg, ?FIXFieldTag_SendingTime),
   ok = fix_parser:set_string_field(FixMsg, ?FIXFieldTag_OrigSendingTime, SendingTime),
   error_logger:info_msg("[~p]: ~p message with MsgSeqNum = ~p will be resent.", [SessionID, FixMsg#msg.type, MsgSeqNum]),
   ok = fix_parser:set_char_field(FixMsg, ?FIXFieldTag_PossDupFlag, $Y),
   {ok, Data1} = resend_fix_message(MsgSeqNum, FixMsg, Data),
   restart_heartbeat_timer(Data1);

'LOGGED_IN'({fix_error, ErrCode, ErrText, Bin}, Data = #data{parser_out = Parser, config =
      #fix_session_config{session_id = SessionID}}) ->
   error_logger:error_msg("[~p]: Error = ~p. Description = ~p.", [SessionID, ErrCode, ErrText]),
   case fix_parser:get_header(Bin, ?FIX_SOH) of
      {ok, #msg_header{msg_seq_num = MsgSeqNum}} ->
         {ok, Reject} = fix_parser:create_msg(Parser, "3"),
         ok = fix_parser:set_int32_field(Reject, ?FIXFieldTag_RefSeqNum, MsgSeqNum),
         ok = fix_parser:set_string_field(Reject, ?FIXFieldTag_Text, ErrText),
         {ok, Data1} = send_fix_message(Reject, Data),
         restart_heartbeat_timer(Data1);
      _ ->
         error_logger:error_msg("Unable to create SessionLevelReject. Message is garbled. Bin = ~p",
            [Bin])
   end,
   {ok, Data}.

% ================= LOGGED_IN END =====================================================================

% =====================================================================================================
% ==================================== FSM end ========================================================
% =====================================================================================================

apply(Data = #data{module_state = MState, config = #fix_session_config{session_id = SessionID, module = Module}, state = OldState}, Msg) ->
   case (catch erlang:apply(?MODULE, OldState, [Msg, Data])) of
      {ok, NewData = #data{state = NewState}} ->
         if NewState =/= OldState ->
            {ok, MState1} = Module:handle_fix_state(OldState, NewState, MState),
            error_logger:info_msg("[~p]: state changed [~p]->[~p].", [SessionID, OldState, NewState]),
            {ok, NewData#data{module_state = MState1}};
         true ->
            {ok, NewData}
         end;
      Reply = {stop, _Reason, _Data} ->
         Reply;
      {'EXIT', {function_clause, [_]}} ->
         error_logger:warning_msg("[~p]: unsupported state [~p] message [~p].", [SessionID, OldState, Msg]),
         {ok, Data};
      Other ->
         error_logger:error_msg("[~p]: wrong call [~p]", [SessionID, Other]),
         exit({error_wrong_result, Other})
   end.

parse_binary(<<>>, Data = #data{config = #fix_session_config{heartbeat_int = 0}}) ->
   {ok, Data};

parse_binary(<<>>, Data = #data{config = #fix_session_config{heartbeat_int = HeartbeatInt, transmission_time = TransTime}}) ->
   restart_timeout_timer(timeout, HeartbeatInt * 1000 + TransTime, Data);

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
      {fix_error, ?FIX_ERROR_NO_MORE_DATA, _} ->
         {ok, Data#data{binary = <<PrefixBin/binary, Bin/binary>>}};
      {fix_error, ErrCode, ErrText} ->
         error_logger:error_msg("[~p]: unable to parse incoming message. Error = [~p], Description = [~p].",
            [Data#data.config#fix_session_config.session_id, ErrCode, ErrText]),
         ?MODULE:apply(Data, {fix_error, ErrCode, ErrText})
   end.

cancel_heartbeat_timer(Data = #data{heartbeat_timer_ref = undefined}) ->
   {ok, Data};

cancel_heartbeat_timer(Data = #data{heartbeat_timer_ref = TimerRef}) ->
   erlang:cancel_timer(TimerRef),
   {ok, Data#data{heartbeat_timer_ref = undefined}}.

restart_heartbeat_timer(Data = #data{config = #fix_session_config{heartbeat_int = 0}}) ->
   {ok, Data};
restart_heartbeat_timer(Data = #data{heartbeat_timer_ref = undefined, config = #fix_session_config{heartbeat_int = Timeout}}) ->
   TimerRef = erlang:start_timer(Timeout * 1000, self(), heartbeat),
   {ok, Data#data{heartbeat_timer_ref = TimerRef}};
restart_heartbeat_timer(Data = #data{heartbeat_timer_ref = OldTimerRef, config = #fix_session_config{heartbeat_int = Timeout}}) ->
   erlang:cancel_timer(OldTimerRef),
   TimerRef = erlang:start_timer(Timeout * 1000, self(), heartbeat),
   {ok, Data#data{heartbeat_timer_ref = TimerRef}}.

cancel_timeout_timer(Data = #data{timeout_timer_ref = undefined}) ->
   {ok, Data};
cancel_timeout_timer(Data = #data{timeout_timer_ref = TimerRef}) ->
   erlang:cancel_timer(TimerRef),
   {ok, Data#data{timeout_timer_ref = undefined}}.

restart_timeout_timer(Type, Timeout, Data = #data{timeout_timer_ref = undefined}) ->
   TimerRef = erlang:start_timer(Timeout, self(), Type),
   {ok, Data#data{timeout_timer_ref = TimerRef}};
restart_timeout_timer(Type, Timeout, Data = #data{timeout_timer_ref = OldTimerRef}) ->
   erlang:cancel_timer(OldTimerRef),
   TimerRef = erlang:start_timer(Timeout, self(), Type),
   {ok, Data#data{timeout_timer_ref = TimerRef}}.

validate_logon(Msg = #msg{type = "A"}, Username, Password) ->
   {ok, Username1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Username, ""),
   {ok, Password1} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Password, ""),
   if ((Username == Username1) andalso (Password == Password1)) -> ok;
      true ->
         throw({error, "Wrong Username/Password"})
   end;
validate_logon(_, _, _) ->
   throw({error, "First message not a logon"}).

create_logout(Text, #data{parser_out = Parser}) ->
   {ok, Msg} = fix_parser:create_msg(Parser, "5"),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_Text, Text),
   {ok, Msg}.

create_logon(ResetSeqNum, #data{parser_out = Parser, config = #fix_session_config{heartbeat_int = HeartBtInt}}) ->
   {ok, Msg} = fix_parser:create_msg(Parser, "A"),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_HeartBtInt, HeartBtInt),
   ok = fix_parser:set_char_field(Msg, ?FIXFieldTag_ResetSeqNumFlag, ResetSeqNum),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_EncryptMethod, 0),
   {ok, Msg}.

reject_logon(Reason, Data) ->
   {ok, LogoutMsg} = create_logout(Reason, Data),
   {ok, Data1} = send_fix_message([LogoutMsg], Data),
   socket_close(Data1).

create_resend(BeginSeqNo, EndSeqNo, #data{parser_out = Parser}) ->
   {ok, Msg} = fix_parser:create_msg(Parser, "2"),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_BeginSeqNo, BeginSeqNo),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_EndSeqNo, EndSeqNo),
   {ok, Msg}.

resend_fix_message(MsgSeqNum, Msg, Data) ->
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SenderCompID, Data#data.config#fix_session_config.sender_comp_id),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_TargetCompID, Data#data.config#fix_session_config.target_comp_id),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_MsgSeqNum, MsgSeqNum),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SendingTime, fix_utils:now_utc()),
   {ok, BinMsg} = fix_parser:msg_to_binary(Msg, ?FIX_SOH),
   case socket_send(Data#data.socket, BinMsg) of
      ok ->
         trace(Msg, out, Data);
      {error, Reason} ->
         error_logger:error_msg("[~p]: unable to resend. Error = ~p.", [Data#data.config#fix_session_config.session_id, Reason])
   end,
   {ok, Data}.

send_fix_message([], Data) ->
   {ok, Data};
send_fix_message([Msg|Rest], Data = #data{config = #fix_session_config{module = Module}, module_state = MState, seq_num_out = SeqNumOut}) ->
   MsgSeqNum = SeqNumOut + 1,
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SenderCompID, Data#data.config#fix_session_config.sender_comp_id),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_TargetCompID, Data#data.config#fix_session_config.target_comp_id),
   ok = fix_parser:set_int32_field(Msg, ?FIXFieldTag_MsgSeqNum, MsgSeqNum),
   ok = fix_parser:set_string_field(Msg, ?FIXFieldTag_SendingTime, fix_utils:now_utc()),
   case fix_parser:msg_to_binary(Msg, ?FIX_SOH) of
      {ok, BinMsg} ->
         case socket_send(Data#data.socket, BinMsg) of
            ok ->
               trace(Msg, out, Data),
               {ok, Data1} = store_msg_out(MsgSeqNum, Msg#msg.type, BinMsg, Data),
               send_fix_message(Rest, Data1);
            {error, Reason} ->
               {ok, MState1} = Module:handle_error({unable_to_send, Reason}, Msg, MState),
               send_fix_message(Rest, Data#data{module_state = MState1})
         end;
      Err = {fix_error, _, _} ->
         {ok, MState1} = Module:handle_error(Err, Msg, MState),
         send_fix_message(Rest, Data#data{module_state = MState1})
   end.

trace(_, _, #data{config = #fix_session_config{tracer = undefined}}) ->
   ok;

trace(Msg, Direction, #data{config = #fix_session_config{tracer = Tracer}}) ->
   fix_tracer:trace(Tracer, Direction, Msg).

store_msg_out(SeqNumOut, _Type, _, Data = #data{config = #fix_session_config{storage = undefined}}) ->
   {ok, Data#data{seq_num_out = SeqNumOut}};

store_msg_out(SeqNumOut, Type, Msg, Data = #data{config = #fix_session_config{storage = Storage}}) ->
   ok = fix_storage:store_message(Storage, SeqNumOut, Type, Msg),
   {ok, Data#data{seq_num_out = SeqNumOut}}.

store_seq_num_in(SeqNumIn, Data = #data{config = #fix_session_config{storage = undefined}}) ->
   {ok, Data#data{seq_num_in = SeqNumIn}};

store_seq_num_in(SeqNumIn, Data = #data{config = #fix_session_config{storage = Storage}}) ->
   ok = fix_storage:set_metadata(Storage, seq_num_in, SeqNumIn),
   {ok, Data#data{seq_num_in = SeqNumIn}}.

get_storage_stat(Data = #data{config = #fix_session_config{storage = undefined}}) ->
   {ok, Data#data{seq_num_in = 0, seq_num_out = 0}};

get_storage_stat(Data = #data{config = #fix_session_config{storage = Storage}}) ->
   {ok, SeqNumIn} = fix_storage:get_metadata(Storage, seq_num_in),
   {ok, SeqNumOut} = fix_storage:get_metadata(Storage, seq_num_out),
   {ok, Data#data{seq_num_in = SeqNumIn, seq_num_out = SeqNumOut}}.

reset_storage(Data = #data{config = #fix_session_config{storage = undefined}}) ->
   {ok, Data#data{seq_num_in = 0, seq_num_out = 0}};

reset_storage(Data = #data{config = #fix_session_config{storage = Storage}}) ->
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
           Type == "2" orelse Type == "4" orelse Type == "1" ->
   resend(Rest, {SeqNum, SeqNum}, Data);

resend([{SeqNum, Type, _BinMsg}|Rest], {SkipFrom, _}, Data)
      when Type == "0" orelse Type == "A" orelse Type == "5" orelse
           Type == "2" orelse Type == "4" orelse Type == "1" ->
   resend(Rest, {SkipFrom, SeqNum}, Data);

resend([{SeqNum, _Type, BinMsg}|Rest], {0, 0}, Data = #data{config = #fix_session_config{module = Module}, module_state = MState}) ->
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

resend([{SeqNum, _Type, BinMsg}|Rest], {SkipFrom, SkipTo}, Data = #data{config = #fix_session_config{module = Module}, module_state = MState}) ->
   {ok, Msg, <<>>} = fix_parser:binary_to_msg(Data#data.parser_out, ?FIX_SOH, BinMsg),
   case Module:handle_resend(Msg, MState) of
      {true, MState1} ->
         {ok, Data1} = ?MODULE:apply(Data, {skip_resend, SkipFrom, SkipTo}),
         {ok, Data2} = ?MODULE:apply(Data1, {resend, Msg});
      {false, MState1} ->
         {ok, Data2} = ?MODULE:apply(Data, {skip_resend, SkipFrom, SeqNum})
   end,
   resend(Rest, {0, 0}, Data2#data{module_state = MState1}).

check_gap(Msg, Fun, Data = #data{config = #fix_session_config{session_id = SessionID}, seq_num_in = SeqNumIn}) ->
   {ok, MsgSeqNum} = fix_parser:get_int32_field(Msg, ?FIXFieldTag_MsgSeqNum),
   {ok, PossDupFlag} = fix_parser:get_char_field(Msg, ?FIXFieldTag_PossDupFlag, $N),
   case MsgSeqNum =:= SeqNumIn + 1 of
      true ->
         Fun();
      false when MsgSeqNum > SeqNumIn + 1 ->
         error_logger:info_msg(
            "[~p]: message ~p with MsgSeqNum = ~p higher whan expected ~p. ResendRequest will be sent.",
            [SessionID, Msg#msg.type, MsgSeqNum, SeqNumIn + 1]),
         {ok, ResendRequest} = create_resend(SeqNumIn + 1, 0, Data),
         {ok, Data1} = send_fix_message([ResendRequest], Data),
         restart_heartbeat_timer(Data1);
      false when MsgSeqNum < SeqNumIn + 1 andalso PossDupFlag =:= $N ->
         {ok, Logout} = create_logout(
            io_lib:format("MsgSeqNum too low, expecting ~p but received ~p", [SeqNumIn, MsgSeqNum]), Data),
         send_fix_message([Logout], Data),
         {ok, Data1} = cancel_heartbeat_timer(Data),
         {ok, Data2} = socket_close(Data1#data.socket),
         {ok, Data2#data{state = 'CONNECTED', socket = undefined}};
      false when MsgSeqNum < SeqNumIn + 1 andalso PossDupFlag =:= $Y ->
         error_logger:info_msg(
            "[~p]: message ~p with PossDupFlag is set and MsgSeqNum = ~p has been skipped. Expected MsgSeqNum = ~p.",
            [SessionID, Msg#msg.type, MsgSeqNum, SeqNumIn + 1]),
         restart_heartbeat_timer(Data)
   end.
