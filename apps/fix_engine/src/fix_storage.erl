-module(fix_storage).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([reset/1, store/3]).

-record(state, {}).

reset(SessionID) ->
   gen_server:cast(fix_storage, {reset, SessionID}).

store(SessionID, MsgSeqNum, Msg) ->
   gen_server:cast(fix_storage, {store, SessionID, MsgSeqNum, Msg}).

start_link(SessionCfg = #fix_session_config{storage = Storage}) ->
   gen_server:start_link({local, Storage}, ?MODULE, SessionCfg, []).

init(_SessionCfg) ->
   {ok, #state{}}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
