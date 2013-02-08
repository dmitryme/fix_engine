-module(session_acceptor).

-include("fix_parser.hrl").

-export([start_link/3, loop/3]).

start_link(Socket, Acceptors, Timeout) ->
   spawn_link(fun() -> loop(Socket, Acceptors, Timeout) end).

loop(Socket, Acceptors, Timeout) ->
   loop(Socket, Acceptors, Timeout, <<>>).

loop(Socket, Acceptors, Timeout, LogonPart) ->
   inet:setopts(Socket, [{active, once}]),
   receive
      {tcp_closed, Socket} ->
         error_logger:info_msg("Peer socket [~p] closed.", [Socket]);
      {tcp_error, Socket, Reason} ->
         error_logger:info_msg("Peer socket [~p] error. Reason = [~p].", [Socket, Reason]),
         gen_tcp:close(Socket);
      {tcp, _, Data} ->
         error_logger:info_msg("Data = [~p]", [Data]),
         Logon = <<LogonPart/binary, Data/binary>>,
         case fix_parser:get_session_id(Logon, ?FIX_SOH) of
            {ok, SenderCompID, TargetCompID} ->
               SessionID = fix_utils:make_session_id(TargetCompID,  SenderCompID),
               error_logger:info_msg("New incoming session [~p] detected.", [SessionID]),
               case ets:lookup(Acceptors, SessionID) of
                  [SessionPid] -> ok;
                  [] ->
                     SessionPid = undef,
                     error_logger:error_msg("No such session [~p] configured.", [SessionID]),
                     gen_tcp:close(Socket)
               end,
               case gen_fix_session:send_logon(SessionPid, Socket, Logon, Timeout) of
                  {error, noproc} ->
                     error_logger:warning_msg("No such session [~p]", [SessionID]),
                     gen_tcp:close(Socket);
                  ok ->
                     gen_tcp:controlling_process(Socket, SessionPid),
                     ok
               end;
            {error, ?FIX_ERROR_BODY_TOO_SHORT, _} ->
               ?MODULE:loop(Socket, Acceptors, Timeout, Logon);
            {error, ErrCode, ErrText} ->
               error_logger:error_msg("Unable to parse logon message. ErrCode = [~p], ErrText = [~p], Logon message = [~p]",
                  [ErrCode, ErrText, Logon]),
               gen_tcp:close(Socket)
         end
   end.
