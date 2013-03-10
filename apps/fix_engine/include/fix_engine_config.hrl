
-define(def_socket_opts, [
      {reuseaddr, true},
      {delay_send, false},
      {nodelay, true},
      {keepalive, true}]).

-record(fix_session_config,
   {
      type                               :: acceptor | initiator,
      module                             :: atom(),
      module_args          = []          :: term(),
      sender_comp_id                     :: string(),
      target_comp_id                     :: string(),
      session_id                         :: atom(),          % don't set it. will be filled by gen_fix_engine
      host                               :: string(),
      port                               :: pos_integer(),
      username             = []          :: string(),
      password             = []          :: string(),
      fix_protocol                       :: string(),        % will be derived from parent #fix_engine_config.fix_protocol
      tracer                             :: atom(),          % don't set it. will be filled by gen_fix_enine
      tracer_module                      :: atom(),          % will be derived from parent #fix_engine_config.tracer_module
      tracer_dir                         :: string(),        % will be derived from parent #fix_engine_config.tracer_dir
      tracer_flags                       :: [term()],        % will be derived from parent #fix_engine_config.tracer_flags
      storage                            :: atom(),          % don't set it. will be filled by gen_fix_engine
      storage_module                     :: atom(),          % will be derived from parent#fix_engine_config.storade_module
      storage_dir                        :: string(),        % will be derived from parent #fix_engine_config.storage_dir
      storage_flags                      :: [term()],        % will be derived from parent #fix_engine_config.storade_flags
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
      tracer_module     = null               :: atom(),      % use null, if you don't want to use tracer
      tracer_dir        = "."                :: string(),
      tracer_flags      = []                 :: [term()],
      storage_module    = null               :: atom(),      % use null, if you don't want use recovery
      storage_dir       = "."                :: string(),
      storage_flags     = []                 :: [term()],
      socket_opts       = ?def_socket_opts   :: [gen_tcp:option()],
      sessions                               :: [#fix_session_config{}]
   }).
