-module(fix_server).

-include_lib("fix_engine/include/fix_parser.hrl").
-include_lib("fix_engine/include/fix_fields.hrl").
-include_lib("fix_engine/include/fix_engine_config.hrl").

-export([
      start_link/1,
      init/4,
      handle_call/3,
      handle_cast/2,
      handle_info/2,
      handle_fix/2,
      handle_fix_state/3,
      handle_resend/2,
      handle_error/3,
      terminate/2,
      code_change/3
   ]).

-behaviour(gen_fix_session).

-record(state, {session_id, parser_ref, order_id = 1, reply}).

start_link(SessionCfg) ->
   gen_fix_session:start_link(SessionCfg, []).

init(SessionID, _FixState, ParserRef, _ModuleArgs) ->
   {ok, Reply} = fix_parser:create_msg(ParserRef, "8"),
   {connect, #state{session_id = SessionID, parser_ref = ParserRef, reply = Reply}}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

handle_info(_Request, State) ->
   {noreply, State}.

handle_fix(FixMsg = #msg{type = "D"}, State = #state{reply = Reply}) ->
   {ok, ClOrdID} = fix_parser:get_string_field(FixMsg, ?FIXFieldTag_ClOrdID),
   {ok, Side} = fix_parser:get_char_field(FixMsg, ?FIXFieldTag_Side),
   {ok, Symbol} = fix_parser:get_string_field(FixMsg, ?FIXFieldTag_Symbol),
   OrderID = "OID_" ++ integer_to_list(State#state.order_id),
   ok = fix_parser:set_string_field(Reply, ?FIXFieldTag_OrderID, OrderID),
   ok = fix_parser:set_string_field(Reply, ?FIXFieldTag_ExecID, "123"),
   ok = fix_parser:set_char_field(Reply, ?FIXFieldTag_ExecType, $0),
   ok = fix_parser:set_char_field(Reply, ?FIXFieldTag_OrdStatus, $0),
   ok = fix_parser:set_char_field(Reply, ?FIXFieldTag_Side, Side),
   ok = fix_parser:set_string_field(Reply, ?FIXFieldTag_ClOrdID, ClOrdID),
   ok = fix_parser:set_string_field(Reply, ?FIXFieldTag_Symbol, Symbol),
   ok = fix_parser:set_double_field(Reply, ?FIXFieldTag_LeavesQty, 100),
   ok = fix_parser:set_double_field(Reply, ?FIXFieldTag_CumQty, 0),
   ok = fix_parser:set_double_field(Reply, ?FIXFieldTag_AvgPx, 0),
   {reply, Reply, State#state{order_id = State#state.order_id + 1}};
handle_fix(FixMsg, State) ->
   {ok, Msg} = fix_parser:msg_to_binary(FixMsg, ?FIX_SOH),
   error_logger:error_msg("Unsupported msg received [~p]", [Msg]),
   {noreply, State}.

handle_fix_state(_OldFixState, _NewFixState, State) ->
   {ok, State}.

handle_resend(_FixMsg, State) ->
   {true, State}.

handle_error(E, _Msg, State) ->
   error_logger:error_msg("ERROR: ~p", [E]),
   {ok, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
