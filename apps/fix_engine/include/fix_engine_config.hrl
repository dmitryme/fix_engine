-record(fix_session_acceptor_config,
   {
      module                           :: atom(),
      senderCompID                     :: string(),
      targetCompID                     :: string(),
      username          = []           :: string(),
      password          = []           :: string(),
      fix_protocol                     :: string(),
      useTracer         = false        :: boolean(),
      fix_parser_flags  = [check_all]  :: [atom()]
   }).

-record(fix_session_initiator_config,
   {
      module                           :: atom(),
      senderCompID                     :: string(),
      targetCompID                     :: string(),
      username      = []               :: string(),
      password      = []               :: string(),
      fix_protocol                     :: string(),
      host                             :: string(),
      port                             :: pos_integer(),
      useTracer     = false            :: boolean(),
      fix_parser_flags = [check_all]   :: [atom()]
   }).

-record(fix_engine_config,
   {
      listenPort        :: pos_integer(),
      tracerDir   = "." :: string(),
      sessions          :: [#fix_session_acceptor_config{}|#fix_session_initiator_config{}]
   }).
