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
%% @private
-module(sighandler).
-include("sighandler.hrl").

-export([start/0, stop/0, install/2, remove/1, remove/2, int/1, atom/1]).

start() -> application:start(sighandler).
stop() -> application:stop(sighandler).

install(Sig, Fun) when is_atom(Sig) andalso ?SH_IS_FUN(Fun) ->
    case int(Sig) of
	{ok, SigInt} -> install(SigInt, Fun);
	{error, _Reason} = Error -> Error
    end;
install(Sig, Fun) when ?SH_IS_SIG_FUN(Sig, Fun) ->
    sighandler_server:install(Sig, Fun).

remove(FunRef) when ?SH_IS_FUNREF(FunRef) ->
    sighandler_server:remove(FunRef).

remove(Sig, FunRef) when is_atom(Sig) andalso ?SH_IS_FUNREF(FunRef) ->
    case int(Sig) of
	{ok, SigInt} -> remove(SigInt, FunRef);
	{error, _Reason} = Error -> Error
    end;
remove(Sig, FunRef) when ?SH_IS_SIG_FUNREF(Sig, FunRef) ->
    sighandler_server:remove(Sig, FunRef).

int(Atom) when is_atom(Atom) -> sighandler_server:lookup_term(Atom).

atom(Int) when is_integer(Int) -> sighandler_server:lookup_term(Int).
