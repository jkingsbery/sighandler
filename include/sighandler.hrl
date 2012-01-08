-ifndef('__sighandler.hrl__').
-define('__sighandler.hrl__', ok).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SH_IS_SIG(S), (is_integer(S))).
-define(SH_IS_FUN(F), (is_function(F, 0) orelse is_function(F, 1))).
-define(SH_IS_FUNREF(F), (?SH_IS_FUN(F) orelse is_reference(F))).
-define(SH_IS_SIG_FUN(S,F), (?SH_IS_SIG(S) andalso ?SH_IS_FUN(F))).
-define(SH_IS_SIG_FUNREF(S,F), (?SH_IS_SIG(S) andalso ?SH_IS_FUNREF(F))).

-endif.
