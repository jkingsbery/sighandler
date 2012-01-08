#!/usr/bin/env escript
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Andrew Tunnell-Jones. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-define(SIGS,
	["SIGABRT", "SIGALRM", "SIGBUS", "SIGCANCEL", "SIGCHLD", "SIGCONT",
	 "SIGEMT", "SIGFPE", "SIGFREEZE", "SIGHUP", "SIGILL", "SIGINFO",
	 "SIGINT", "SIGIO", "SIGIOT", "SIGKILL", "SIGLOST", "SIGLWP", "SIGPIPE",
	 "SIGPOLL", "SIGPROF", "SIGPWR", "SIGQUIT", "SIGRTMAX", "SIGRTMIN",
	 "SIGSEGV", "SIGSTKSZ", "SIGSTOP", "SIGSYS", "SIGTERM", "SIGTHAW",
	 "SIGTRAP", "SIGTSTP", "SIGTTIN", "SIGTTOU", "SIGURG", "SIGUSR1",
	 "SIGUSR2", "SIGVTALRM", "SIGWAITING", "SIGWINCH", "SIGXCPU",
	 "SIGXFSZ"]
       ).

-define(ERL_HEAD,
"-module(sighandler_nif).
-compile([export_all]).
-on_load(on_load/0).
on_load() ->
    So = case code:priv_dir(sighandler) of
             {error, bad_name} ->
                 ModPath = filename:dirname(code:which(?MODULE)),
                 filename:join(ModPath, \"../priv/\" ?MODULE_STRING);
             Dir ->
                 filename:join(Dir, ?MODULE_STRING)
         end,
    case erlang:load_nif(So, 0) of
        {error, {load_failed, Reason}} ->
	    Format = ?MODULE_STRING \" load NIF failed:~n~p~n\",
	    error_logger:warning_msg(Format, [Reason]);
        ok -> ok
    end.
").

fun_name(Sig) -> string:to_lower(lists:sublist(Sig, 4, length(Sig))).

gen_erl() ->
    [
     ?ERL_HEAD,
     [ io_lib:format("~s() -> undefined.~n", [fun_name(Sig)]) || Sig <- ?SIGS ]
    ].

gen_c() ->
    [
     "#include <signal.h>\n#include \"erl_nif.h\"\n",
     [ io_lib:format(
	 "static ERL_NIF_TERM "
	 "sh_~s(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {~n"
	 "\t#ifdef ~s~n"
	 "\treturn enif_make_int(env, ~s);~n"
	 "\t#else~n"
	 "\treturn enif_make_atom(env, \"undefined\");~n"
	 "\t#endif~n"
	 "}~n",
	 [fun_name(Sig), Sig, Sig]) || Sig <- ?SIGS ],
     "static ErlNifFunc nif_funcs[] = {\n",
     lists:map(fun(Sig) ->
		       Prefix = case get(done_first) =:= undefined of
				    true -> put(done_first, true), "";
				    false -> ",\n"
				end,
		       io_lib:format("~s\t{\"~s\", 0, sh_~s}",
				     [Prefix, fun_name(Sig), fun_name(Sig)])
	       end, ?SIGS),
     "\n};\n"
     "ERL_NIF_INIT(sighandler_nif, nif_funcs, NULL,NULL, NULL, NULL);\n"
    ].

main([OutputDirPrefix]) ->
    NifErl = filename:join([OutputDirPrefix, "src", "sighandler_nif.erl"]),
    NifC = filename:join([OutputDirPrefix, "c_src", "sighandler_nif.c"]),
    ok = file:write_file(NifErl, gen_erl()),
    ok = file:write_file(NifC, gen_c()).
