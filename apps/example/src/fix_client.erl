-module(fix_client).

-include_lib("fix_parser/include/fix_parser.hrl").
-include_lib("fix_parser/include/fix_fields.hrl").
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

-record(state, {session_id, parser_ref}).

start_link(SessionCfg) ->
   gen_fix_session:start_link(SessionCfg, []).

init(SessionID, _FixState, ParserRef, _ModuleArgs) ->
   {connect, #state{session_id = SessionID, parser_ref = ParserRef}}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

handle_info(_Request, State) ->
   {noreply, State}.

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
