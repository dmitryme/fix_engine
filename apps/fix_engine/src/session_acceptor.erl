-module(session_acceptor).

-include("fix_parser.hrl").

-export([start_link/2, loop/2, set_socket/2]).

start_link(Acceptors, Timeout) ->
   spawn_link(fun() -> loop(Acceptors, Timeout) end).

set_socket(Pid, Socket) ->
   Pid ! {set_socket, Socket}.

loop(Acceptors, Timeout) ->
   receive
      {set_socket, Socket} ->
         loop(Socket, Acceptors, Timeout, <<>>);
      UnknownMessage ->
         error_logger:error_msg("Unknown message [~p] received.", [UnknownMessage])
   end.

loop(Socket, Acceptors, Timeout, LogonPart) ->
   inet:setopts(Socket, [{active, once}]),
   receive
      {tcp_closed, Socket} ->
         error_logger:info_msg("Peer socket [~p] closed.", [Socket]);
      {tcp_error, Socket, Reason} ->
         error_logger:info_msg("Peer socket [~p] error. Reason = [~p].", [Socket, Reason]),
         gen_tcp:close(Socket);
      {tcp, Socket, Data} ->
         Logon = <<LogonPart/binary, Data/binary>>,
         case fix_parser:get_session_id(Logon, ?FIX_SOH) of
            {ok, SenderCompID, TargetCompID} ->
               SessionID = fix_utils:make_session_id(TargetCompID,  SenderCompID),
               error_logger:info_msg("New incoming session [~p] detected.", [SessionID]),
               case ets:lookup(Acceptors, SessionID) of
                  [{SessionID, SessionPid}] ->
                     gen_tcp:controlling_process(Socket, SessionPid),
                     gen_fix_session:send_logon(SessionPid, Socket, Logon, Timeout);
                  [] ->
                     error_logger:error_msg("No such session [~p] configured.", [SessionID]),
                     gen_tcp:close(Socket)
               end;
            {error, ?FIX_ERROR_BODY_TOO_SHORT, _} ->
               ?MODULE:loop(Socket, Acceptors, Timeout, Logon);
            {error, ErrCode, ErrText} ->
               error_logger:error_msg("Unable to parse logon message. ErrCode = [~p], ErrText = [~p], Logon message = [~p]",
                  [ErrCode, ErrText, Logon]),
               gen_tcp:close(Socket)
         end
   end.
