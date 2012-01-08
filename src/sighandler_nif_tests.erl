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
-module(sighandler_nif_tests).
-include("sighandler.hrl").

-ifdef(TEST).

simple_test_() ->
    [ ?_assert(begin
		   Result = sighandler_nif:X(),
		   is_integer(Result) orelse Result =:= undefined
	       end)
      || {X, 0} <- sighandler_nif:module_info(exports),
	 X =/= on_load, X =/= module_info ].

-endif.
