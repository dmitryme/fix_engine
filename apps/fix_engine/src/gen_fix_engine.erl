-module(gen_fix_engine).

-include("fix_engine_config.hrl").

-behaviour(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, inet_async_ref}).

start_link(Name, Args) ->
   gen_server:start_link(Name, ?MODULE, Args, []).

-spec init(#fix_engine_config{}) -> {ok, term()}.
init(Config = #fix_engine_config{listen_port = undefined}) ->
   init_common(Config),
   {ok, #state{}};
init(Config = #fix_engine_config{listen_port = ListenPort, socket_opts = SocketOpts}) ->
   init_common(Config),
   {Socket, InetAsyncRef} = open_socket(ListenPort, SocketOpts),
   {ok, #state{socket = Socket, inet_async_ref = InetAsyncRef}}.

init_common(Config) ->
   error_logger:info_msg("gen_fix_engine init started."),
   {ok, _SupPid} = fix_engine_sup:start_link(),
   ets:new(fix_acceptors, [named_table]),
   ets:new(fix_initiators, [named_table]),
   ok = create_sessions(Config).

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

handle_info({inet_async, ListenSocket, _Ref, {ok, ClientSocket}}, State) ->
   case inet:peername(ClientSocket) of
      {ok, PeerInfo} -> error_logger:info_msg("New connection [~p] accepted. Socket = [~p].", [PeerInfo, ClientSocket]);
      {error, ErrCode} -> exit({peername_error, ErrCode})
   end,
   inet_db:register_socket(ClientSocket, inet_tcp),
   case prim_inet:getopts(ListenSocket, [active, mode, buffer, delay_send, high_msgq_watermark, high_watermark, keepalive,
            linger, low_msgq_watermark, nodelay, priority, recbuf, reuseaddr, send_timeout, send_timeout_close,
         sndbuf, tos]) of
      {ok, Opts} ->
         error_logger:info_msg("Accepted socket options: ~p.", [Opts]),
         case prim_inet:setopts(ClientSocket, Opts) of
            ok -> ok;
            Error ->
               gen_tcp:close(ClientSocket),
               error_logger:error_msg("Unable to set socket [~p] options ~p. Error = [~p].", [ClientSocket, Opts, Error])
         end;
      Error ->
         error_logger:error_msg("Unable to get socket [~p] options. Error = [~p].", [ListenSocket, Error]),
         gen_tcp:close(ClientSocket)
   end,
   SAPid = fix_session_disp:start_link(1000),
   gen_tcp:controlling_process(ClientSocket, SAPid),
   fix_session_disp:set_socket(SAPid, ClientSocket),
   case prim_inet:async_accept(ListenSocket, -1) of
      {ok, NewRef} -> ok;
      {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
   end,
   {noreply, State#state{inet_async_ref = NewRef}};
handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

open_socket(ListenPort, SocketOptsDirty) when is_number(ListenPort) ->
   error_logger:info_msg("Opening port ~p.", [ListenPort]),
   SocketOpts = [{active, false}, binary, {backlog, 100} | SocketOptsDirty],
   error_logger:info_msg("Listen socket options are: ~p.", [SocketOpts]),
   {ok, ListenSocket} = gen_tcp:listen(ListenPort, SocketOpts),
   case prim_inet:async_accept(ListenSocket, -1) of
      {ok, Ref} -> ok;
      {error, Ref} -> exit({async_accept, inet:format_error(Ref)})
   end,
   {ListenSocket, Ref};
open_socket(ListenPort, _SocketOpts) ->
   error_logger:info_msg("Listen port '~p' not opened.", [ListenPort]).

-spec create_sessions(#fix_engine_config{}) -> ok.
create_sessions(FixEngineCfg) ->
   create_sessions(FixEngineCfg, FixEngineCfg#fix_engine_config.sessions, 0).

-spec create_sessions(#fix_engine_config{}, [#fix_session_initiator_config{}|#fix_session_acceptor_config{}], pos_integer()) -> ok.
create_sessions(_FixEngineCfg, [], 0) ->
   error_logger:warning_msg("No sessions are configured."),
   ok;

create_sessions(_FixEngineCfg, [], _Id) ->
   ok;

create_sessions(FixEngineCfg, [Session = #fix_session_acceptor_config{}|Rest], Id) ->
   SessionID = fix_utils:make_session_id(
            Session#fix_session_acceptor_config.sender_comp_id,
            Session#fix_session_acceptor_config.target_comp_id),
   NewSession = merge_cfg(FixEngineCfg, Session),
   {ok, TracerId} = create_tracer(SessionID, FixEngineCfg, NewSession),
   NewSession1 = NewSession#fix_session_acceptor_config{tracer = TracerId},
   {ok, ChildPid} = supervisor:start_child(
      fix_engine_sup, {
         Id,
         {Session#fix_session_acceptor_config.module, start_link, [NewSession1]},
         permanent,
         brutal_kill,
         worker,
         [Session#fix_session_acceptor_config.module]
      }),
   true = ets:insert(fix_acceptors, {SessionID, ChildPid}),
   create_sessions(FixEngineCfg, Rest, Id + 1);

create_sessions(FixEngineCfg, [Session = #fix_session_initiator_config{}|Rest], Id) ->
   SessionID = fix_utils:make_session_id(
            Session#fix_session_initiator_config.sender_comp_id,
            Session#fix_session_initiator_config.target_comp_id),
   NewSession = merge_cfg(FixEngineCfg, Session),
   {ok, Tracer} = create_tracer(SessionID, FixEngineCfg, NewSession),
   NewSession1 = NewSession#fix_session_initiator_config{tracer = Tracer},
   {ok, ChildPid} = supervisor:start_child(
      fix_engine_sup, {
         Id,
         {Session#fix_session_initiator_config.module, start_link, [NewSession1]},
         permanent,
         brutal_kill,
         worker,
         [Session#fix_session_initiator_config.module]
      }),
   true = ets:insert(fix_initiators, {SessionID, ChildPid}),
   create_sessions(FixEngineCfg, Rest, Id + 1).

merge_cfg(#fix_engine_config{tracer_type = TType1, storage_type = SType1, storage_flags = SFlags1},
      FixSessionCfg = #fix_session_initiator_config{tracer_type = TType2, storage_type = SType2,
         storage_flags = SFlags2}) ->
   FixSessionCfg#fix_session_initiator_config{
      tracer_type = merge_param(TType2, TType1),
      storage_type = merge_param(SType2, SType1),
      storage_flags = merge_param(SFlags2, SFlags1)};

merge_cfg(#fix_engine_config{tracer_type = TType1, storage_type = SType1, storage_flags = SFlags1},
      FixSessionCfg = #fix_session_acceptor_config{tracer_type = TType2, storage_type = SType2,
         storage_flags = SFlags2}) ->
   FixSessionCfg#fix_session_acceptor_config{
      tracer_type = merge_param(TType2, TType1),
      storage_type = merge_param(SType2, SType1),
      storage_flags = merge_param(SFlags2, SFlags1)}.

merge_param(undef, A) ->
   A;
merge_param(B, _) ->
   B.

create_tracer(_, _, #fix_session_acceptor_config{tracer_type = null}) ->
   {ok, undef};
create_tracer(_, _, #fix_session_initiator_config{tracer_type = null}) ->
   {ok, undef};
create_tracer(SessionID, #fix_engine_config{tracer_dir = Dir}, #fix_session_acceptor_config{tracer_type = TType}) ->
   Tracer = fix_utils:list_to_atom("fix_tracer_" ++ atom_to_list(SessionID)),
   {ok, _Pid} = supervisor:start_child(
      fix_engine_sup, {
         Tracer,
         {fix_tracer, start_link, [{Tracer, Dir, TType, SessionID}]},
         permanent,
         brutal_kill,
         worker,
         [fix_tracer]}),
   {ok, Tracer};
create_tracer(SessionID, #fix_engine_config{tracer_dir = Dir}, #fix_session_initiator_config{tracer_type = TType}) ->
   Tracer = fix_utils:list_to_atom("fix_tracer_" ++ atom_to_list(SessionID)),
   {ok, _Pid} = supervisor:start_child(
      fix_engine_sup, {
         Tracer,
         {fix_tracer, start_link, [{Tracer, Dir, TType, SessionID}]},
         permanent,
         brutal_kill,
         worker,
         [fix_tracer]}),
   {ok, Tracer}.

create_storage(_) -> {ok, undef}.
%create_storage(_S)->
   %{ok, _Pid} = supervisor:start_child(
      %fix_engine_sup, {
         %fix_storage,
         %{fix_storage, start_link, [[]]},
         %permanent,
         %brutal_kill,
         %worker,
         %[fix_storage]}).
