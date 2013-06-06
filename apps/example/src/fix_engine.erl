-module(fix_engine).

-include("../..//include/fix_engine_config.hrl").

-compile([export_all]).

configuration() ->
   #fix_engine_config{
      listen_port = 60000,
      tracer_module = fix_tracer_file,
      tracer_dir = "./trace",
      storage_module = fix_storage_dets,
      storage_dir = "./storage",
      storage_flags = [{estimated_no_objects, 1000000}],
      fix_protocol = "fix.4.4.xml",
      sessions = [
         #fix_session_config{
            type = acceptor,
            module = fix_server,
            sender_comp_id = "server",
            target_comp_id = "client",
            username = "",
            password = ""
         },
         #fix_session_config{
            type = initiator,
            module = fix_client,
            sender_comp_id = "client",
            target_comp_id = "server",
            username = "",
            password = "",
            host = "localhost",
            port = 60000,
            heartbeat_int = 1
         }
       ]
   }.

start_link() ->
   gen_fix_engine:start_link({local, fix_engine}, configuration()).
