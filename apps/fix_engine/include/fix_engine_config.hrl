-record(fix_session_acceptor_config,
   {
      module                             :: atom(),
      sender_comp_id                     :: string(),
      target_comp_id                     :: string(),
      username             = []          :: string(),
      password             = []          :: string(),
      fix_protocol                       :: string(),
      use_tracer           = false       :: boolean(),
      fix_parser_flags     = [check_all] :: [atom()],
      lost_heartbeat_delta = 500         :: pos_integer()    % in milliseconds. Heartbeat considered lost if no messages
                                                             % is received after HeartBtInt * 1000 + lost_heartbeat_delta
   }).

-record(fix_session_initiator_config,
   {
      module                             :: atom(),
      sender_comp_id                     :: string(),
      target_comp_id                     :: string(),
      username             = []          :: string(),
      password             = []          :: string(),
      fix_protocol                       :: string(),
      host                               :: string(),
      port                               :: pos_integer(),
      use_tracer           = false       :: boolean(),
      fix_parser_flags     = [check_all] :: [atom()],
      reconnect_interval   = 1000        :: pos_integer(),   % in milliseconds
      reconnect_attempts   = 10          :: pos_integer(),
      lost_heartbeat_delta = 500         :: pos_integer()    % in milliseconds. Heartbeat considered lost if no messages
                                                             % is received after HeartBtInt * 1000 + lost_heartbeat_delta
   }).

-record(fix_engine_config,
   {
      listen_port                :: pos_integer(),
      tracer_dir        = "."    :: string(),
      storade_dir       = "."    :: string(),
      storage_type      = dets   :: atom(),         % dets, ets, null
      storage_flags     = []     :: [term()],
      sessions                   :: [#fix_session_acceptor_config{}|#fix_session_initiator_config{}]
   }).
