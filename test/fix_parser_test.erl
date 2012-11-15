-module(fix_parser_test).

-compile([export_all]).

init() ->
   {ok, P} = fix_parser:create("deps/fix_parser/fix_descr/fix.4.4.perf.xml", [], []),
   {ok, M} = fix_parser:create_msg(P, "8"),
   M.

test(M) ->
   ok = fix_parser:set_string_field(M, 49, "QWERTY_12345678"),
   ok = fix_parser:set_string_field(M, 56, "ABCQWE_XYZ"),
   ok = fix_parser:set_int32_field(M, 34, 34),
   ok = fix_parser:set_string_field(M, 57, "srv-ivanov_ii1"),
   ok = fix_parser:set_string_field(M, 52, "20120716-06:00:16.230"),
   ok = fix_parser:set_string_field(M, 37, "1"),
   ok = fix_parser:set_string_field(M, 11, "CL_ORD_ID_1234567"),
   ok = fix_parser:set_string_field(M, 17, "FE_1_9494_1"),
   ok = fix_parser:set_char_field(M, 150, $0),
   ok = fix_parser:set_char_field(M, 39, $1),
   ok = fix_parser:set_string_field(M, 1, "ZUM"),
   ok = fix_parser:set_string_field(M, 55, "RTS-12.12"),
   ok = fix_parser:set_char_field(M, 54, $1),
   ok = fix_parser:set_double_field(M, 38, 25),
   ok = fix_parser:set_double_field(M, 44, 135155.0),
   ok = fix_parser:set_char_field(M, 59, $0),
   ok = fix_parser:set_double_field(M, 32, 0),
   ok = fix_parser:set_double_field(M, 31, 0),
   ok = fix_parser:set_double_field(M, 151, 25.0),
   ok = fix_parser:set_double_field(M, 14, 0),
   ok = fix_parser:set_double_field(M, 6, 0),
   ok = fix_parser:set_char_field(M, 21, $1),
   ok = fix_parser:set_string_field(M, 58, "COMMENT12"),
   fix_parser:msg_to_string(M, $|).