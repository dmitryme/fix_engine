-module(gen_fix_engine).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {supervisor}).

-spec init(#fix_engine_acceptor_config{}) -> {ok, term()}.
init(Args = #fix_engine_acceptor_config{}) ->
   {ok, Supervisor} = supervisor:start_link(erlang_fix_sup, []),
   ok = create_sessions(Args#fix_engine_acceptor_config.sessions, Supervisor, 0),
   {ok, #state{supervisor = Supervisor}}.

-spec init(#fix_engine_initiator_config{}) -> {ok, term()}.
init(Args = #fix_engine_initiator_config{}) ->
   {ok, Supervisor} = supervisor:start_link(erlang_fix_sup, []),
   ok = create_sessions(Args#fix_engine_initiator_config.sessions, Supervisor, 0),
   {ok, #state{supervisor = Supervisor}}.

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

-spec create_sessions([#fix_session_initiator_config{}|#fix_session_acceptor_config()], pid(), pos_integer()) -> ok.
create_sessions([], _Supervisor, 0) ->
   error_logger:warning_msg("No sessions are configured."),
   ok;
create_sessions([], _Supervisor, _Id) ->
   ok;
create_sessions([#|Rest], Supervisor, Id) ->
   {ok, _Child} = supervisor:start_child(
      Supervisor, {
         Id,
         {Session#fix_session_config.module, start_link, [Session]},
         permanent,
         brutal_kill,
         worker,
         [Session#fix_session_config.module]
      }),
   create_sessions(Rest, Supervisor, Id + 1).
