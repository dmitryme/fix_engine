-module(gen_fix_engine).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {supervisor, acceptors, initiators}).

start_link(Name, Args) ->
   gen_server:start_link(Name, ?MODULE, Args, []).

-spec init(#fix_engine_config{}) -> {ok, term()}.
init(Args = #fix_engine_config{}) ->
   {ok, Supervisor} = supervisor:start_link(erlang_fix_sup, []),
   Acceptors = ets:new(acceptors, []),
   Initiators = ets:new(initiators, []),
   ok = create_sessions(Args#fix_engine_config.sessions, Supervisor, 0, Acceptors, Initiators),
   {ok, #state{supervisor = Supervisor, acceptors = Acceptors, initiators = Initiators}}.

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

-spec create_sessions([#fix_session_initiator_config{}|#fix_session_acceptor_config{}], pid(), pos_integer(), term(),
   term()) -> ok.
create_sessions([], _Supervisor, 0, _, _) ->
   error_logger:warning_msg("No sessions are configured."),
   ok;
create_sessions([], _Supervisor, _Id, _, _) ->
   ok;
create_sessions([Session = #fix_session_acceptor_config{}|Rest], Supervisor, Id, Acceptors, Initiators) ->
   {ok, Child} = supervisor:start_child(
      Supervisor, {
         Id,
         {Session#fix_session_acceptor_config.module, start_link, [Session]},
         permanent,
         brutal_kill,
         worker,
         [Session#fix_session_acceptor_config.module]
      }),
   true = ets:insert(
      Acceptors, {
         fix_utils:make_session_id(
            Session#fix_session_acceptor_config.senderCompID,
            Session#fix_session_acceptor_config.targetCompID), Child}),
   create_sessions(Rest, Supervisor, Id + 1, Acceptors, Initiators);
create_sessions([Session = #fix_session_initiator_config{}|Rest], Supervisor, Id, Acceptors, Initiators) ->
   {ok, Child} = supervisor:start_child(
      Supervisor, {
         Id,
         {Session#fix_session_initiator_config.module, start_link, [Session]},
         permanent,
         brutal_kill,
         worker,
         [Session#fix_session_initiator_config.module]
      }),
   true = ets:insert(
      Initiators, {
         fix_utils:make_session_id(
            Session#fix_session_initiator_config.senderCompID,
            Session#fix_session_initiator_config.targetCompID), Child}),
   create_sessions(Rest, Supervisor, Id + 1, Acceptors, Initiators).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

test1_test() ->
   {ok, Supervisor} = supervisor:start_link(erlang_fix_sup, []),
   Acceptors = ets:new(acceptors, []),
   Initiators = ets:new(initiators, []),
   Sessions = [
         #fix_session_acceptor_config{
            module = gen_fix_session,
            senderCompID = "SERVER1",
            targetCompID = "CLIENT1",
            username = "user1",
            password = "passwd",
            fix_protocol = "fix4.4.xml"
         }],
   create_sessions(Sessions, Supervisor, 0, Acceptors, Initiators).

-endif.
