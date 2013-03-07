
-define(def_socket_opts, [
      {reuseaddr, true},
      {delay_send, false},
      {nodelay, true},
      {keepalive, true}]).

-record(fix_session_acceptor_config,
   {
      module                             :: atom(),
      module_args          = []          :: term(),
      sender_comp_id                     :: string(),
      target_comp_id                     :: string(),
      username             = []          :: string(),
      password             = []          :: string(),
      fix_protocol                       :: string(),
      use_tracer           = false       :: boolean(),
      tracer_dir           = undef       :: string(),
      storade_dir          = undef       :: string(),        % will be derived from parent #fix_engine_config.storade_dir
      storage_type         = undef       :: atom(),          % will be derived from parent #fix_engine_config.storade_type
      storage_flags        = undef       :: [term()],        % will be derived from parent #fix_engine_config.storade_flags
      fix_parser_flags     = [check_all] :: [atom()],
      lost_heartbeat_delta = 500         :: pos_integer()    % in milliseconds. Heartbeat considered lost if no messages
                                                             % is received after HeartBtInt * 1000 + lost_heartbeat_delta
   }).

-record(fix_session_initiator_config,
   {
      module                             :: atom(),
      module_args          = []          :: term(),
      sender_comp_id                     :: string(),
      target_comp_id                     :: string(),
      username             = []          :: string(),
      password             = []          :: string(),
      fix_protocol                       :: string(),
      host                               :: string(),
      port                               :: pos_integer(),
      use_tracer           = false       :: boolean(),
      tracer_dir           = undef       :: string(),
      storade_dir          = undef       :: string(),        % will be derived from parent #fix_engine_config.storade_dir
      storage_type         = undef       :: atom(),          % will be derived from parent #fix_engine_config.storade_type
      storage_flags        = undef       :: [term()],        % will be derived from parent #fix_engine_config.storade_flags
      fix_parser_flags     = [check_all] :: [atom()],
      reconnect_interval   = 1000        :: pos_integer(),   % in milliseconds
      reconnect_attempts   = 10          :: pos_integer(),
      lost_heartbeat_delta = 500         :: pos_integer()    % in milliseconds. Heartbeat considered lost if no messages
                                                             % is received after HeartBtInt * 1000 + lost_heartbeat_delta
   }).

-record(fix_engine_config,
   {
      listen_port                            :: pos_integer(),
      tracer_dir        = "."                :: string(),
      storade_dir       = "."                :: string(),
      storage_type      = dets               :: atom(),         % dets, ets, null
      storage_flags     = []                 :: [term()],
      socket_opts       = ?def_socket_opts   :: [gen_tcp:option()],
      sessions                               :: [#fix_session_acceptor_config{}|#fix_session_initiator_config{}]
   }).
