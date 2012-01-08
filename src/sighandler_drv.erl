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
-module(sighandler_drv).
-include("sighandler.hrl").

-export([open/0, close/1, status/2, toggle/2]).

-define(DRIVER_NAME, ?MODULE_STRING).

open() ->
    ok = load(),
    erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary]).

close(Port) when is_port(Port) ->
    case erlang:port_info(Port) of
	undefined -> ok;
	_ ->
	    true = erlang:port_close(Port),
	    ok = unload()
    end.

status(Port, Sig) when is_port(Port) andalso ?SH_IS_SIG(Sig) ->
    erlang:port_call(Port, Sig, status).

toggle(Port, Sig) when is_port(Port) andalso ?SH_IS_SIG(Sig) ->
    erlang:port_call(Port, Sig, toggle).

%%%===================================================================
%%% Internal functions
%%%===================================================================

load() ->
    {ok, Drivers} = erl_ddll:loaded_drivers(),
    case lists:member(?DRIVER_NAME, Drivers) of
	true -> ok;
	false ->
	    case erl_ddll:load(priv_dir(), ?DRIVER_NAME) of
		ok -> ok;
		{error, Reason} = Error ->
		    error_logger:error_msg(
		      ?MODULE_STRING ": Error loading ~p: ~p~n",
		      [?DRIVER_NAME, erl_ddll:format_error(Reason)]
		     ),
		    Error
	    end
    end.

unload() ->
    case erl_ddll:unload_driver(?DRIVER_NAME) of
	ok -> ok;
	{error, Reason} = Error ->
	    error_logger:error_msg(
	      ?MODULE_STRING ": Error unloading ~p: ~p~n",
	      [?DRIVER_NAME, erl_ddll:format_error(Reason)]
	     ),
	    Error
    end.


priv_dir() ->
    case code:priv_dir(sighandler) of
	List when is_list(List) -> List;
	{error, bad_name} ->
	    filename:join(filename:dirname(code:which(?MODULE)), "../priv")
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

drv_test_() ->
    [?_assertEqual(ok, load()),
     ?_assertEqual(ok, unload())].

-endif.
