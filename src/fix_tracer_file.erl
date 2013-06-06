-module(fix_tracer_file).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {fdescr}).

start_link(SessionCfg = #fix_session_config{tracer = Tracer}) ->
   gen_server:start_link({local, Tracer}, ?MODULE, SessionCfg, []).

init(#fix_session_config{session_id = SessionID, tracer_dir = Dir}) ->
   ok = fix_utils:make_dir(Dir),
   FName = filename:join([Dir, atom_to_list(SessionID) ++ ".tracer"]),
   {ok, FDescr} = file:open(FName, [write, raw, append]),
   error_logger:info_msg("[~p]: tracer file ~p has been opened.", [SessionID, FName]),
   file:write(FDescr, [fix_utils:now(), <<" START TRACE ('->' - incoming messages, '<-' - outgoing messages)\n">>]),
   {ok, #state{fdescr = FDescr}}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast({Direction, UNow, Msg}, State) ->
   {ok, BinMsg} = fix_parser:msg_to_binary(Msg, $|),
   file:write(State#state.fdescr, [print_direction(Direction), <<" ">>, UNow, <<" ">>, BinMsg, <<"\n">>]),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

print_direction(in) ->
   <<"->">>;
print_direction(out) ->
   <<"<-">>.
