-module(gen_fix_acceptor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("fix_engine/include/fix_engine_config.hrl").
-include_lib("fix_engine/include/fix_parser.hrl").

-compile([export_all]).

receive_all(Timeout, Acc) ->
   receive
      Msg ->
         receive_all(Timeout, [Msg|Acc])
   after
      Timeout ->
         lists:reverse(Acc)
   end.

expected(Timeout, PatternList) ->
   Msgs = receive_all(Timeout, []),
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
         ct:fail("Pattern ~p unknown return. Should be at least 'ok'", [Counter])
   end.


all() ->
   [disconnected_connect_test,
    disconnected_logon_test,
    disconnected_bad_msg_test,
    connected_timeout_test,
    connected_bad_msg_test].

init_per_suite(Config) ->
   Config.

end_per_suite(Config) ->
   Config.

init_per_testcase(TestCase, Config) ->
   SessionCfg = #fix_session_acceptor_config{
      module = gen_fix_acceptor,
      sender_comp_id = "server",
      target_comp_id = "client",
      username = "",
      password = "",
      fix_protocol ="../../../../deps/fix_parser/fix_descr/fix.4.4.xml",
      use_tracer = true
   },
   {ok, Pid} = gen_fix_acceptor:start_link(SessionCfg),
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
