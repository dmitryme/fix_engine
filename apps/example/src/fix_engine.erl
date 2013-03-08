-module(fix_engine).

-include_lib("fix_engine/include/fix_engine_config.hrl").

-compile([export_all]).

configuration() ->
   #fix_engine_config{
      listen_port = 60000,
      tracer_dir = "./trace",
      tracer_type = file,
      fix_protocol = "../deps/fix_parser/fix_descr/fix.4.4.xml",
      sessions = [
         #fix_session_acceptor_config{
            module = fix_server,
            sender_comp_id = "server",
            target_comp_id = "client",
            username = "",
            password = ""
         },
         #fix_session_acceptor_config{
            module = fix_server,
            sender_comp_id = "server",
            target_comp_id = "client1",
            username = "",
            password = ""
         }
       ]
   }.

start_link() ->
   gen_fix_engine:start_link({local, fix_engine}, configuration()).
