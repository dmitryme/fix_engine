-module(gen_fix_acceptor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("fix_engine/include/fix_engine_config.hrl").
-include_lib("fix_engine/include/fix_parser.hrl").
-include_lib("fix_engine/include/fix_fields.hrl").

-behaviour(gen_fix_acceptor).

-compile([export_all]).

-export([init/3, handle_call/3, handle_cast/2, handle_info/2, handle_fix/2, terminate/2, code_change/3]).

init(_SessionID, _ParserRef, _ModuleArgs) ->
   {ok, []}.

handle_call(_Request, _From, State) ->
   {reply, ok, State}.

handle_cast(_Request, State) ->
   {noreply, State}.

handle_info(_Request, State) ->
   {noreply, State}.

handle_fix(_FixMsg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

receive_all(_Timeout, 0, Acc) ->
   lists:reverse(Acc);
receive_all(Timeout, ExpectedMsgCont, Acc) ->
   receive
      Msg ->
         receive_all(Timeout, ExpectedMsgCont - 1, [Msg|Acc])
   after
      Timeout ->
         lists:reverse(Acc)
   end.

expected(Timeout, PatternList) ->
   Msgs = receive_all(Timeout, length(PatternList), []),
   case (length(Msgs) == length(PatternList)) of
      true ->
         check(Msgs, PatternList, 1);
      false ->
         ct:fail("Msgs =/= Patterns")
   end.

check([], [], _) ->
   ok;
check([Msg|Msgs], [Pattern|PatternList], Counter) ->
   case (catch Pattern(Msg)) of
      {'EXIT', {function_clause, _}} ->
         ct:fail("Message [~p] doesn't match to ~p pattern", [Msg, Counter]);
      failed ->
         ok;
      ok ->
         check(Msgs, PatternList, Counter + 1);
      Ret ->
         ct:fail("Pattern ~p unknown return ~p. Stack = [~p]. Should be at least 'ok'", [Counter, Ret,
               erlang:get_stacktrace()])
   end.


all() ->
   [disconnected_connect_test,
      disconnected_logon_test,
      disconnected_bad_msg_test,
      connected_timeout_test,
      connected_bad_msg_test,
      connected_not_logon_msg_test,
      connected_invalid_password_msg_test,
      connected_invalid_username_msg_test,
      connected_valid_logon_msg_test].

init_per_suite(Config) ->
   Config.

end_per_suite(Config) ->
   Config.

init_per_testcase(TestCase, Config) ->
   SessionCfg = #fix_session_acceptor_config{
      module = gen_fix_acceptor_SUITE,
      sender_comp_id = "server",
      target_comp_id = "client",
      username = "user",
      password = "password",
      fix_protocol ="../../../../deps/fix_parser/fix_descr/fix.4.4.xml",
      tracer_type = null,
      storage_type = null
   },
   {ok, Pid} = gen_fix_acceptor:start_link(SessionCfg, []),
   gen_fix_acceptor:set_socket(Pid, self()),
   {ok, ParserRef} = fix_parser:create("../../../../deps/fix_parser/fix_descr/fix.4.4.xml", [], []),
   [{parser_ref, ParserRef}, {session_pid, Pid} | Config].

end_per_testcase(TestCase, Config) ->
   Config.

disconnected_connect_test(_Config) ->
   gen_fix_acceptor:connect(server_client),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   gen_fix_acceptor:disconnect(server_client),
   'DISCONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   ok.

disconnected_logon_test(Config)->
   SessionPid = proplists:get_value(session_pid, Config),
   LogonBin = <<"8=FIX.4.4\0019=72\00135=A\00149=client\00156=server\00134=1\00152=20130301-10:07:06.558\00198=0\001108=1\001141=Y\00110=010\001">>,
   SessionPid ! {tcp, self(), LogonBin},
   expected(1000,
      [
         fun(tcp_close) -> ok end
      ]),
   'DISCONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   ok.

disconnected_bad_msg_test(Config) ->
   SessionPid = proplists:get_value(session_pid, Config),
   LogonBin = <<"8=FIX.4.4\0019=\00135=A">>,
   SessionPid ! {tcp, self(), LogonBin},
   expected(1000,
      [
         fun(tcp_close) -> ok end
      ]),
   'DISCONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   ok.

connected_timeout_test(Config) ->
   gen_fix_acceptor:connect(server_client),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   SessionPid = proplists:get_value(session_pid, Config),
   SessionPid ! {timeout, self(), heartbeat},
   expected(1000, []),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   ok.

connected_bad_msg_test(Config) ->
   ParserRef = proplists:get_value(parser_ref, Config),
   gen_fix_acceptor:connect(server_client),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   SessionPid = proplists:get_value(session_pid, Config),
   LogonBin = <<"8=FIX.4.4\0019=\00135=A">>,
   SessionPid ! {tcp, self(), LogonBin},
   expected(1000,
      [
         fun(Bin) when is_binary(Bin) ->
            {ok, Msg = #msg{type = "5"}, _} = fix_parser:binary_to_msg(ParserRef, ?FIX_SOH, Bin),
            ok
         end,
         fun(tcp_close) -> ok end
      ]),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   ok.

connected_not_logon_msg_test(Config) ->
   ParserRef = proplists:get_value(parser_ref, Config),
   gen_fix_acceptor:connect(server_client),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   SessionPid = proplists:get_value(session_pid, Config),
   LogonBin = <<"8=FIX.4.4\0019=56\00135=0\00149=server\00156=client\00134=96\00152=20130304-11:24:20.617\00110=032\001">>,
   SessionPid ! {tcp, self(), LogonBin},
   expected(1000,
      [
         fun(Bin) when is_binary(Bin) ->
            {ok, Msg = #msg{type = "5"}, _} = fix_parser:binary_to_msg(ParserRef, ?FIX_SOH, Bin),
            {ok, "First message not a logon"} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Text),
            ok
         end,
         fun(tcp_close) -> ok end
      ]),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   ok.

connected_invalid_password_msg_test(Config) ->
   ParserRef = proplists:get_value(parser_ref, Config),
   gen_fix_acceptor:connect(server_client),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   LogonBin = create_logon(ParserRef, "client", "server", 1, 60, "user", "password1"),
   SessionPid = proplists:get_value(session_pid, Config),
   SessionPid ! {tcp, self(), LogonBin},
   expected(1000,
      [
         fun(Bin) when is_binary(Bin) ->
            {ok, Msg = #msg{type = "5"}, _} = fix_parser:binary_to_msg(ParserRef, ?FIX_SOH, Bin),
            {ok, "Wrong Username/Password"} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Text),
            ok
         end,
         fun(tcp_close) -> ok end
      ]),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client).

connected_invalid_username_msg_test(Config) ->
   ParserRef = proplists:get_value(parser_ref, Config),
   gen_fix_acceptor:connect(server_client),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   LogonBin = create_logon(ParserRef, "client", "server", 1, 60, "user1", "password"),
   SessionPid = proplists:get_value(session_pid, Config),
   SessionPid ! {tcp, self(), LogonBin},
   expected(1000,
      [
         fun(Bin) when is_binary(Bin) ->
            {ok, Msg = #msg{type = "5"}, _} = fix_parser:binary_to_msg(ParserRef, ?FIX_SOH, Bin),
            {ok, "Wrong Username/Password"} = fix_parser:get_string_field(Msg, ?FIXFieldTag_Text),
            ok
         end,
         fun(tcp_close) -> ok end
      ]),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client).

connected_valid_logon_msg_test(Config) ->
   ParserRef = proplists:get_value(parser_ref, Config),
   gen_fix_acceptor:connect(server_client),
   'CONNECTED' == gen_fix_acceptor:get_current_state(server_client),
   LogonBin = create_logon(ParserRef, "client", "server", 1, 60, "user", "password"),
   SessionPid = proplists:get_value(session_pid, Config),
   SessionPid ! {tcp, self(), LogonBin},
   expected(1000,
      [
         fun(Bin) when is_binary(Bin) ->
            {ok, Msg = #msg{type = "A"}, _} = fix_parser:binary_to_msg(ParserRef, ?FIX_SOH, Bin),
            ok
         end
      ]),
   'LOGGED_IN' == gen_fix_acceptor:get_current_state(server_client).


create_logon(ParserRef, SenderCompID, TargetCompID, MsgSeqNum, HeartBtInt, Username, Password) ->
   {ok, Logon} = fix_parser:create_msg(ParserRef, "A"),
   ok = fix_parser:set_string_field(Logon, ?FIXFieldTag_SenderCompID, SenderCompID),
   ok = fix_parser:set_string_field(Logon, ?FIXFieldTag_TargetCompID, TargetCompID),
   ok = fix_parser:set_int32_field(Logon, ?FIXFieldTag_MsgSeqNum, MsgSeqNum),
   ok = fix_parser:set_string_field(Logon, ?FIXFieldTag_SendingTime, fix_utils:now_utc()),
   ok = fix_parser:set_int32_field(Logon, ?FIXFieldTag_EncryptMethod, 0),
   ok = fix_parser:set_int32_field(Logon, ?FIXFieldTag_HeartBtInt, HeartBtInt),
   ok = fix_parser:set_string_field(Logon, ?FIXFieldTag_Username, Username),
   ok = fix_parser:set_string_field(Logon, ?FIXFieldTag_Password, Password),
   {ok, LogonBin} = fix_parser:msg_to_binary(Logon, ?FIX_SOH),
   LogonBin.
