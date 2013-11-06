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
-module(sighandler_server).
-behaviour(gen_server).
-include("sighandler.hrl").

%% API
-export([start_link/0, stop/0, install/2, remove/1, remove/2, lookup_term/1, registered/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port :: port(), handlers = dict:new()}).

-define(SERVER, ?MODULE).
-define(TAB_TERM, sighandler_term).

%%%===================================================================
%%% API
%%%===================================================================

install(Sig, Fun) when ?SH_IS_SIG_FUN(Sig, Fun) ->
    gen_server:call(?SERVER, {install, Sig, Fun}).

remove(FunRef) when ?SH_IS_FUNREF(FunRef) ->
    gen_server:call(?SERVER, {remove, FunRef}).

remove(Sig, FunRef) when ?SH_IS_SIG_FUNREF(Sig, FunRef) ->
    gen_server:call(?SERVER, {remove, Sig, FunRef}).

lookup_term(Term) when is_atom(Term) orelse is_integer(Term) ->
    case (catch ets:lookup_element(?TAB_TERM, Term, 2)) of
	{'EXIT', _} -> {error, undefined};
	Other -> {ok, Other}
    end.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

registered()-> 
    State=gen_server:call(?SERVER,registered),
    State#state.handlers.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = setup_sig_tab(),
    Port = sighandler_drv:open(),
    {ok, #state{port = Port}}.

handle_call({install, Sig, Fun}, _From, #state{} = State) ->
    Ref = make_ref(),
    {Result, State0} = install(Sig, Fun, Ref, State),
    Reply = case Result of
		ok -> {ok, Ref};
		{error, _Reason} = Error -> Error
	    end,
    {reply, Reply, State0};
handle_call({remove, FunRef}, _From, #state{} = State) ->
    {ok, State0} = find_and_remove(FunRef, State),
    {reply, ok, State0};
handle_call({remove, Sig, Fun}, _From, #state{} = State) ->
    {ok, State0} = remove(Sig, Fun, State),
    {reply, ok, State0};
handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call(registered,_From,State)->
    {reply, State,State};
handle_call(Request, _From, State) ->
    error_logger:info_msg(?MODULE_STRING " ~p ignoring call: ~p~n",
			  [self(), Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    error_logger:info_msg(?MODULE_STRING " ~p ignoring cast: ~p~n",
			  [self(), Msg]),
    {noreply, State}.

handle_info({Port, Sig}, #state{port = Port} = State)
  when is_integer(Sig) ->
    {ok, State0} = fire(Sig, State),
    {noreply, State0};
handle_info(Info, State) ->
    error_logger:info_msg(?MODULE_STRING " ~p discarded message: ~p~n",
			  [self(), Info]),
    {noreply, State}.

terminate(_Reason, #state{port = Port} = _State) ->
    ok = sighandler_drv:close(Port).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% sig_tab

setup_sig_tab() ->
    ?TAB_TERM = ets:new(?TAB_TERM, [named_table, protected]),
    Fun = fun({Atom, 0}) when Atom =/= on_load andalso Atom =/= module_info ->
		  case sighandler_nif:Atom() of
		      Int when is_integer(Int) ->
			  Terms = [{Int, Atom}, {Atom, Int}],
			  true = ets:insert(?TAB_TERM, Terms);
		      undefined -> ok
		  end;
	     (_) -> ok end,
    lists:foreach(Fun, sighandler_nif:module_info(exports)).

%% State management
install(Sig, Fun, Ref, #state{handlers = Handlers, port = Port} = State) ->
    Installed = case sighandler_drv:status(Port, Sig) of
		    active -> installed;
		    inactive -> sighandler_drv:toggle(Port, Sig);
		    Other -> Other
		end,
    case Installed of
	installed ->
	    Handlers0 = dict:append(Sig, {Ref, Fun}, Handlers),
	    {ok, State#state{handlers = Handlers0}};
	sig_err = Err -> {{error, Err}, State}
    end.

remove(Sig, FunRef, #state{handlers = Handlers, port = Port} = State) ->
    case dict:is_key(Sig, Handlers) of
	true ->
	    Funs = dict:fetch(Sig, Handlers),
	    case lists:keydelete(FunRef, remove_pos(FunRef), Funs) of
		[] ->
		    Handlers0 = dict:erase(Sig, Handlers),
		    removed = sighandler_drv:toggle(Port, Sig),
		    {ok, State#state{handlers = Handlers0}};
		Funs -> {ok, State};
		Funs0 ->
		    Handlers0 = dict:store(Sig, Funs0, Handlers),
		    {ok, State#state{handlers = Handlers0}}
	    end;
	false -> {ok, State}
    end.

find_and_remove(FunRef, #state{handlers = Handlers} = State) ->
    Pos = remove_pos(FunRef),
    Sigs = dict:fold(fun(Sig, Funs, Acc) ->
			     case lists:keymember(FunRef, Pos, Funs) of
				 true -> [Sig|Acc];
				 false -> Acc
			     end
		     end, [], Handlers),
    find_and_remove(Sigs, FunRef, State).

find_and_remove([Sig|Sigs], FunRef, State) ->
    {ok, State0} = remove(Sig, FunRef, State),
    find_and_remove(Sigs, FunRef, State0);
find_and_remove([], _FunRef, State) -> {ok, State}.

remove_pos(Ref) when is_reference(Ref) -> 1;
remove_pos(Fun) when is_function(Fun) -> 2.

fire(Sig, #state{handlers = Handlers} = State) ->
    case dict:is_key(Sig, Handlers) of
	true ->
	    Funs = dict:fetch(Sig, Handlers),
	    fire(Funs, Sig, State);
	false -> {ok, State}
    end.

fire([{Ref, Fun}|Funs], Sig, #state{} = State) ->
    Run = fun(F) when is_function(F, 0) -> catch F();
	     (F) when is_function(F, 1) -> catch F(Sig) end,
    case Run(Fun) of
	ok -> fire(Funs, Sig, State);
	_ ->
	    {ok, State0} = remove(Sig, Ref, State),
	    fire(Funs, Sig, State0)
    end;
fire([], _Sig, #state{} = State) -> {ok, State}.

%% tests

-ifdef(TEST).

fire_test() ->
    ok = application:start(sighandler),
    Sig = 1,
    Tab = ets:new(sh_test, [public]),
    Fun = fun(X) ->
		  io:format("fire_test ~p~n", [X]),
		  ets:insert(Tab, {X}),
		  ok
	  end,
    {ok, Ref} = sighandler:install(Sig, Fun),
    _ = os:cmd(io_lib:format("kill -~p ~s", [Sig, os:getpid()])),
    ok = sighandler:remove(Sig, Ref),
    Result = (catch ets:lookup(Tab, Sig)),
    ets:delete(Tab),
    ok = application:stop(sighandler),
    ?assertEqual([{Sig}], Result).

fire_badfun_test() ->
    ok = application:start(sighandler),
    Sig = 1,
    Cmd = io_lib:format("kill -~p ~s", [Sig, os:getpid()]),
    Tab = ets:new(sh_test, [public]),
    true = ets:insert(Tab, {Sig, 0}),
    FunA = fun(X) ->
		   io:format("fire_badfun_test a ~p~n", [X]),
		   ets:update_counter(Tab, X, 1),
		   ok
	   end,
    {ok, Ref0} = sighandler:install(Sig, FunA),
    FunB = fun(X) ->
		   io:format("fire_badfun_test b ~p~n", [X]),
		   ets:update_counter(Tab, X, 1),
		   throw(bad)
	   end,
    {ok, Ref1} = sighandler:install(Sig, FunB),
    _ = os:cmd(Cmd),
    _ = os:cmd(Cmd),
    ok = sighandler:remove(Sig, Ref0),
    ok = sighandler:remove(Sig, Ref1),
    Result = (catch ets:lookup(Tab, Sig)),
    ets:delete(Tab),
    ok = application:stop(sighandler),
    ?assertEqual([{Sig, 3}], Result).

find_and_remove_ref_test() ->
    ok = application:start(sighandler),
    	     Sig = 1,
    Cmd = io_lib:format("kill -~p ~s", [Sig, os:getpid()]),
    Tab = ets:new(sh_test, [public]),
    true = ets:insert(Tab, {Sig, 0}),
    Fun = fun(X) ->
		  io:format("find_and_remove_ref_test ~p~n", [X]),
		  ets:update_counter(Tab, X, 1),
		  ok
	  end,
    {ok, Ref0} = sighandler:install(Sig, Fun),
    {ok, Ref1} = sighandler:install(Sig, Fun),
    ok = sighandler:remove(Sig, Ref0),
    _ = os:cmd(Cmd),
    _ = os:cmd(Cmd),
    ok = sighandler:remove(Ref1),
    Result = (catch ets:lookup(Tab, Sig)),
    ets:delete(Tab),
    ok = application:stop(sighandler),
    ?assertEqual([{Sig, 2}], Result).

find_and_remove_fun_test() ->
    ok = application:start(sighandler),
    Sig = 1,
    Cmd = io_lib:format("kill -~p ~s", [Sig, os:getpid()]),
    Tab = ets:new(sh_test, [public]),
    true = ets:insert(Tab, {Sig, 0}),
    Fun0 = fun(X) ->
		   io:format("find_and_remove_fun_test a ~p~n", [X]),
		   ets:update_counter(Tab, X, 1),
		   ok
	   end,
    Fun1 = fun(X) ->
		   io:format("find_and_remove_fun_test b ~p~n", [X]),
		   ets:update_counter(Tab, X, 1),
		   ok
	   end,
    {ok, Ref0} = sighandler:install(Sig, Fun0),
    {ok, _Ref1} = sighandler:install(Sig, Fun1),
    _ = os:cmd(Cmd),
    ok = sighandler:remove(Fun1),
    _ = os:cmd(Cmd),
    ok = sighandler:remove(Sig, Ref0),
    Result = (catch ets:lookup(Tab, Sig)),
    ets:delete(Tab),
    ok = application:stop(sighandler),
    ?assertEqual([{Sig, 3}], Result).

-endif.
