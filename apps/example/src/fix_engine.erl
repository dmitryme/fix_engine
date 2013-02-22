-module(fix_engine).

-include_lib("fix_engine/include/fix_engine_config.hrl").

-compile([export_all]).

configuration() ->
   #fix_engine_config{
      listenPort = 60000,
      tracerDir = "./trace",
      sessions = [
         #fix_session_acceptor_config{
            module = gen_fix_acceptor,
            senderCompID = "server",
            targetCompID = "client",
            username = "",
            password = "",
            fix_protocol = "../deps/fix_parser/fix_descr/fix.4.4.xml",
            useTracer = true
         }]
   }.

start_link() ->
   gen_fix_engine:start_link({local, fix_engine}, configuration()).
