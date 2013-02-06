-module(fix_client).

-include_lib("fix_engine/include/fix_engine_config.hrl").

-compile([export_all]).

configuration() ->
   #fix_engine_config{
      sessions = [
         #fix_session_initiator_config{
            module = gen_fix_session,
            senderCompID = "CLIENT1",
            targetCompID = "SERVER1",
            username = "user1",
            password = "passwd",
            fix_protocol = "../deps/fix_parser/fix_descr/fix.4.4.xml"
         }]
   }.

start_link() ->
   gen_fix_engine:start_link({local, fix_client}, configuration()).
