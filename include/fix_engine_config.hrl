-record(fix_session_acceptor_config,
   {
      module :: atom(),
      senderCompID :: string(),
      targetCompID :: string(),
      username     :: string(),
      password     :: string(),
      fix_protocol :: string()
   }).

-record(fix_session_initiator_config,
   {
      module :: atom(),
      senderCompID :: string(),
      targetCompID :: string(),
      username     :: string(),
      password     :: string(),
      fix_protocol :: string(),
      host         :: string(),
      port         :: pos_integer()
   }).

-record(fix_engine_acceptor_config,
   {
      listenPort :: pos_integer(),
      sessions :: [#fix_session_acceptor_config{}]
   }).

-record(fix_engine_initiator_config,
   {
      sessions :: [#fix_session_initiator_config{}]
   }).
