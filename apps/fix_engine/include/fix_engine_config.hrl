
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
      fix_protocol         = undef       :: string(),      % will be derived from parent #fix_engine_config.fix_protocol
      tracer               = undef       :: atom(),
      tracer_type          = undef       :: atom(),        % will be derived from parent #fix_engine_config.tracer_type
      storage              = undef       :: atom(),
      storage_type         = undef       :: atom(),        % will be derived from parent #fix_engine_config.storade_type
      storage_flags        = undef       :: [term()],      % will be derived from parent #fix_engine_config.storade_flags
      fix_parser_flags     = [check_all] :: [atom()],
      lost_heartbeat_delta = 500         :: pos_integer()  % in milliseconds. Heartbeat considered lost if no messages
                                                           % is received after HeartBtInt * 1000 + lost_heartbeat_delta
   }).

-record(fix_session_initiator_config,
   {
      module                             :: atom(),
      module_args          = []          :: term(),
      sender_comp_id                     :: string(),
      target_comp_id                     :: string(),
      host                               :: string(),
      port                               :: pos_integer(),
      username             = []          :: string(),
      password             = []          :: string(),
      fix_protocol         = undef       :: string(),        % will be derived from parent #fix_engine_config.fix_protocol
      tracer               = undef       :: atom(),
      tracer_type          = undef       :: atom(),          % will be derived from parent #fix_engine_config.tracer_type
      storage              = undef       :: atom(),
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
      fix_protocol                           :: string(),    % path to XML with FIX protocol description
      tracer_type       = null               :: null | file,
      tracer_dir        = "."                :: string(),
      storage_dir       = "."                :: string(),
      storage_type      = dets               :: dets | ets | null,
      storage_flags     = []                 :: [term()],
      socket_opts       = ?def_socket_opts   :: [gen_tcp:option()],
      sessions                               :: [#fix_session_acceptor_config{}|#fix_session_initiator_config{}]
   }).
