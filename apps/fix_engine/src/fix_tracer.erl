-module(fix_tracer).

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([trace/3]).

-record(state, {out_dir, fdescr}).

trace(SessionID, Direction, Msg) ->
   gen_server:cast(fix_tracer, {SessionID, Direction, fix_utils:unow(), Msg}).

start_link(Args) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Dir) ->
   case file:make_dir(Dir) of
      ok ->
         ok;
      {error, eexist} ->
         ok;
      Error ->
         exit(Error)
   end,
   error_logger:info_msg("Directory ~p has been created.", [Dir]),
   {ok, #state{out_dir = Dir, fdescr = ets:new(tracer, [])}}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast({SessionID, Direction, UNow, Msg}, State) ->
   case ets:lookup(State#state.fdescr, SessionID) of
      [{SessionID, Descr}] ->
         ok;
      [] ->
         FName = filename:join([State#state.out_dir, atom_to_list(SessionID) ++ ".tracer"]),
         {ok, Descr} = file:open(FName, [write, raw, append]),
         file:write(Descr, [fix_utils:now(), <<" START TRACE ('->' - incoming messages, '<-' - outgoing messages)\n">>]),
         true = ets:insert(State#state.fdescr, {SessionID, Descr})
   end,
   {ok, BinMsg} = fix_parser:msg_to_binary(Msg, $|),
   file:write(Descr, [print_direction(Direction), <<" ">>, UNow, <<" ">>, BinMsg, <<"\n">>]),
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
