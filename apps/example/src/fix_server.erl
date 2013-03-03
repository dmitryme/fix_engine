-module(fix_server).

-include_lib("fix_engine/include/fix_parser.hrl").
-include_lib("fix_engine/include/fix_fields.hrl").
-include_lib("fix_engine/include/fix_engine_config.hrl").

-export([start_link/1, init/3, handle_call/3, handle_cast/2, handle_info/2, handle_fix/2, terminate/2, code_change/3]).

-behaviour(gen_fix_acceptor).

-record(state, {session_id, parser_ref, order_id = 1}).

start_link(SessionCfg) ->
   gen_fix_acceptor:start_link(SessionCfg, []).

init(SessionID, ParserRef, _ModuleArgs) ->
   {connect, #state{session_id = SessionID, parser_ref = ParserRef}}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

handle_info(_Request, State) ->
   {noreply, State}.

handle_fix(FixMsg = #msg{type = "D"}, State) ->
   {ok, ClOrdID} = fix_parser:get_string_field(FixMsg, ?FIXFieldTag_ClOrdID),
   {ok, Side} = fix_parser:get_char_field(FixMsg, ?FIXFieldTag_Side),
   {ok, Symbol} = fix_parser:get_string_field(FixMsg, ?FIXFieldTag_Symbol),
   {ok, Reply} = fix_parser:create_msg(State#state.parser_ref, "8"),
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
   error_logger:error_msg("Unsupported msg received [~p]", [FixMsg#msg.type]),
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
