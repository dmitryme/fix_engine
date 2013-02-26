-module(fix_session_acceptor).

-include("fix_parser.hrl").

-export([start_link/1, loop/1, set_socket/2]).

start_link(Timeout) ->
   spawn_link(fun() -> loop(Timeout) end).

set_socket(Pid, Socket) ->
   Pid ! {set_socket, Socket}.

loop(Timeout) ->
   receive
      {set_socket, Socket} ->
         loop(Socket, Timeout, <<>>);
      UnknownMessage ->
         error_logger:error_msg("Unknown message [~p] received.", [UnknownMessage])
   end.

loop(Socket, Timeout, LogonPart) ->
   inet:setopts(Socket, [{active, once}]),
   receive
      {tcp_closed, Socket} ->
         error_logger:info_msg("Peer socket [~p] closed.", [Socket]);
      {tcp_error, Socket, Reason} ->
         error_logger:info_msg("Peer socket [~p] error. Reason = [~p].", [Socket, Reason]),
         gen_tcp:close(Socket);
      {tcp, Socket, Data} ->
         Logon = <<LogonPart/binary, Data/binary>>,
         case fix_parser:get_header(Logon, ?FIX_SOH) of
            {ok, #msg_header{sender_comp_id = SenderCompID, target_comp_id = TargetCompID} ->
               SessionID = fix_utils:make_session_id(TargetCompID,  SenderCompID),
               error_logger:info_msg("New incoming session [~p] detected.", [SessionID]),
               case ets:lookup(fix_acceptors, SessionID) of
                  [{SessionID, SessionPid}] ->
                     ok = gen_fix_acceptor:set_socket(SessionPid, Socket),
                     SessionPid ! {tcp, Socket, Logon};
                  [] ->
                     error_logger:error_msg("No such session [~p] configured.", [SessionID]),
                     gen_tcp:close(Socket)
               end;
            {error, ?FIX_ERROR_BODY_TOO_SHORT, _} ->
               ?MODULE:loop(Socket, Timeout, Logon);
            {error, ErrCode, ErrText} ->
               error_logger:error_msg("Unable to parse logon message. ErrCode = [~p], ErrText = [~p], Logon message = [~p]",
                  [ErrCode, ErrText, Logon]),
               gen_tcp:close(Socket)
         end
   end.
