-module(fix_server).

-include_lib("fix_engine/include/fix_engine_config.hrl").

-compile([export_all]).

configuration() ->
   #fix_engine_config{
      listenPort = 8001,
      sessions = [
         #fix_session_acceptor_config{
            module = gen_fix_session,
            senderCompID = "SERVER1",
            targetCompID = "CLIENT1",
            username = "user1",
            password = "passwd",
            fix_protocol = "../deps/fix_parser/fix_descr/fix.4.4.xml"
         }]
   }.

start_link() ->
   gen_fix_engine:start_link({local, fix_server}, configuration()).
