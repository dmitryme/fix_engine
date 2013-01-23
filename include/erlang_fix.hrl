-type attr()      :: {'page_size', pos_integer()} |
                     {'max_page_size', pos_integer()} |
                     {'num_pages', pos_integer()} |
                     {'max_pages', pos_integer()} |
                     {'num_groups', pos_integer()} |
                     {'max_groups', pos_integer()}.  %% parser attributes ...
-type attrs()     :: [attr()].

-type flag()      :: check_crc |
                     check_required |
                     check_value |
                     check_unknown_fields |
                     check_all.
-type flags()     :: [flag()].

-record(parser, {res :: binary()}).

-record(msg,    {type :: binary(),
                 res :: {binary(), binary()}}).

-record(group,  {res :: {binary(), binary(), binary()}}).

-type ref()       :: #msg{} | #group{}.
-type tagNum()    :: pos_integer().
-type reason()    :: atom() |
                     string() |
                     {pos_integer(), string()}.
