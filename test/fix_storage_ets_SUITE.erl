-module(fix_storage_ets_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("fix_engine_config.hrl").

-compile([export_all]).

-define(storage_name, client_server_fix_storage_ets).

all() ->
   [
      init_test
      ,metadata_test
      ,store_msg_test
   ].

init_per_suite(Config) ->
   Config.

end_per_suite(Config) ->
   Config.

init_per_testcase(_TestCase, Config) ->
   fix_storage_ets:start_link(#fix_session_config{
         storage = ?storage_name,
         storage_dir=".",
         session_id = server_client,
         storage_flags = []}),
   Config.

end_per_testcase(_TestCase, Config) ->
   Config.

init_test(_Config) ->
   {ok, 0} = fix_storage:get_metadata(?storage_name, seq_num_in),
   {ok, 0} = fix_storage:get_metadata(?storage_name, seq_num_out),
   {ok, false} = fix_storage:get_metadata(?storage_name, correctly_terminated).

metadata_test(_Config) ->
   ok = fix_storage:set_metadata(?storage_name, seq_num_in, 10),
   {ok, 10} = fix_storage:get_metadata(?storage_name, seq_num_in),
   ok = fix_storage:set_metadata(?storage_name, correctly_terminated, true),
   {ok, true} = fix_storage:get_metadata(?storage_name, correctly_terminated).

store_msg_test(_Config) ->
   fix_storage:store_message(?storage_name, 1, "A", msg1),
   fix_storage:store_message(?storage_name, 2, "B", msg2),
   {ok, 2} = fix_storage:get_metadata(?storage_name, seq_num_out),
   ok = fix_storage:get_messages(?storage_name, 1, 2),
   receive
      {resend, [{1, "A", msg1}, {2, "B", msg2}]} -> ok;
      Msg -> io:format("~p~n", [Msg]), 1 = 2
   end,
   fix_storage:store_message(?storage_name, 3, "C", msg3),
   ok = fix_storage:get_messages(?storage_name, 3, 10),
   receive
      {resend, [{3, "C", msg3}]} -> ok;
      Msg1 -> io:format("~p~n", [Msg1]), 1 = 2
   end,
   fix_storage:reset(?storage_name),
   {ok, 0} = fix_storage:get_metadata(?storage_name, seq_num_out).
