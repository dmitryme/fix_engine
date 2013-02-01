-module(session_acceptor).

-include("fix_parser.hrl").

-export([start_link/2, loop/2]).

start_link(Socket, Timeout) ->
   spawn_link(fun() -> loop(Socket, Timeout) end).

loop(Socket, Timeout) ->
   loop(Socket, <<>>, Timeout).

loop(Socket, LogonPart, Timeout) ->
   inets:setopts(Socket, [{active, once}]),
   receive
      {tcp, _, Data} ->
         Logon = <<LogonPart/binary, Data/binary>>,
         case fix_parser:get_session_id(Logon, ?FIX_SOH) of
            {ok, SenderCompID, TargetCompID} ->
               SessionID = fix_utils:list_to_atom(TargetCompID ++ $@ ++ SenderCompID),
               case gen_fix_session:send_logon(SessionID, Socket, Logon, Timeout) of
                  {error, noproc} ->
                     error_logger:warning_msg("No such session [~p]", [SessionID]),
                     gen_tcp:close(Socket);
                  ok ->
                     ok
               end;
            {error, ?FIX_ERROR_BODY_TOO_SHORT, _} ->
               ?MODULE:loop(Socket, Logon);
            {error, ErrCode, ErrText} ->
               error_logger:error_msg("Unable to parse logon message. ErrCode = [~p], ErrText = '~p', LogonMsg = [~p]",
                  [ErrCode, ErrText, Logon]),
               gen_tcp:close(Socket)
         end
   end.
