%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mdns_supervisor).
-behaviour(supervisor).
-define(SERVER, {via, gproc, {n, l, ?MODULE}}).

%% API
-export([
	 start_link/0,
	 start_link/1
	]).

%% Supervisor callbacks
-export([
	 init/1
	]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link([]).

start_link(Parameters) ->
    supervisor:start_link(?SERVER, ?MODULE, Parameters).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Procs = [worker(mdns_node_discovery_server),
	     worker(mdns_node_discovery)],
    {ok, {{one_for_one, 5, 10}, Procs}}.

worker(Module) ->
    worker(Module, permanent).

worker(Module, Restart) ->
    worker(Module, Restart, []).

worker(Module, Restart, Parameters) ->
    worker(Module, Module, Restart, Parameters).    

worker(Id, Module, Restart, Parameters) ->
    {Id, {Module, start_link, Parameters}, Restart, 5000, worker, [Module]}.



