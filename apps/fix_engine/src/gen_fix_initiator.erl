-module(gen_fix_initiator).

-include("fix_engine_config.hrl").
-include("fix_parser.hrl").
-include("fix_fields.hrl").

-behaviour(gen_server).

-export([connect/1, disconnect/1]).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(data, {session_id, socket = undef, use_tracer = false, parser, seq_num_out = 1, se_num_in = 1, sender_comp_id, target_comp_id, username,
      password, state = 'DISCONNECTED', heartbeat_int, timer_ref = undef, binary = <<>>}).

connect(SessionID) ->
   gen_server:call(SessionID, connect).

disconnect(SessionID) ->
   gen_server:call(SessionID, disconnect).

start_link(Args = #fix_session_initiator_config{sender_comp_id = SenderCompID, target_comp_id = TargetCompID}) ->
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   error_logger:info_msg("Starting initiator session [~p].", [SessionID]),
   gen_server:start_link({local, SessionID}, ?MODULE, Args, []).

init(#fix_session_initiator_config{fix_protocol = Protocol, sender_comp_id = SenderCompID, target_comp_id = TargetCompID,
      username = Username, password = Password, use_tracer = UseTracer}) ->
   case fix_parser:create(Protocol, [], []) of
      {ok, ParserRef} -> error_logger:info_msg("Parser [~p] has been created.", [fix_parser:get_version(ParserRef)]);
      {error, ParserRef} -> exit({fix_parser_error, ParserRef})
   end,
   SessionID = fix_utils:make_session_id(SenderCompID, TargetCompID),
   print_use_tracer(SessionID, UseTracer),
   {ok, #data{session_id = SessionID, parser = ParserRef, sender_comp_id = SenderCompID, target_comp_id = TargetCompID,
         username = Username, password = Password, use_tracer = UseTracer}}.

handle_call(connect, _From, Data = #data{state = 'DISCONNECTED'}) ->
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'CONNECTED']),
   {reply, ok, Data#data{state = 'CONNECTED'}};

handle_call(disconnect, _From, Data = #data{state = 'CONNECTED'}) ->
   error_logger:info_msg("[~p] state changed ~p->~p.", [Data#data.session_id, Data#data.state, 'DISCONNECTED']),
   {reply, ok, Data#data{state = 'DISCONNECTED'}};

handle_call(Request, _From, Data) ->
   error_logger:error_msg("Unsupported message [~p].", [Request]),
   {reply, ok, Data}.

handle_cast(_Request, Data) ->
   {noreply, Data}.

handle_info(Info, Data) ->
   error_logger:error_msg("Unsupported message [~p].", [Info]),
   {noreply, Data}.

terminate(_Reason, _Data) ->
   ok.

code_change(_OldVsn, Data, _Extra) ->
   {ok, Data}.

print_use_tracer(SessionID, true) ->
   error_logger:info_msg("Session [~p] will use tracer.", [SessionID]);
print_use_tracer(_SessionID, _) ->
   ok.
