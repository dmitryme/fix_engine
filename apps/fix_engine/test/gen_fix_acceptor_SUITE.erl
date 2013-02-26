-module(gen_fix_acceptor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("fix_engine/include/fix_engine_config.hrl").
-include_lib("fix_engine/include/fix_parser.hrl").

-compile([export_all]).

all() -> [connect_disconnect_test, bad_login_test].

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

connect_disconnect_test(_Config) ->
   gen_fix_acceptor:connect(server_client),
   gen_fix_acceptor:disconnect(server_client),
   ok.

bad_login_test([{parser_ref, ParserRef}, {session_pid, SessionPid} | Config]) ->
   {ok, Logon} = fix_parser:create_msg(ParserRef, "8"),
   {ok, BinLogon} = fix_parser:msg_to_binary(Logon, ?FIX_SOH),
   SessionPid ! {tcp, self(), BinLogon},
   receive
      tcp_close ->
         ok;
      Msg ->
         ct:fail("Wrong message [~p] received.", [Msg])
   after
      1000 ->
         ct:fail("close not received")
   end,
   ok.
