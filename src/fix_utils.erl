-module(fix_utils).

-export([list_to_atom/1, get_property/2, get_property/3]).

list_to_atom(List) ->
   try erlang:list_to_existing_atom(List) of
      Atom -> Atom
   catch
      error:badarg -> erlang:list_to_atom(List)
   end.

get_property(Key, PropList) ->
   case proplists:get_value(Key, PropList) of
      undefined ->
         throw("Configuration parameter" ++ erlang:atom_to_list(Key) ++ " is missing.");
      Value ->
         Value
   end.

get_property(Key, PropList, DefValue) ->
   case proplists:get_value(Key, PropList) of
      undefined ->
         DefValue;
      Value ->
         Value
   end.
