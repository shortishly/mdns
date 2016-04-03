%% Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(mdns_discovery_connect_node_handler).
-behaviour(gen_event).

-export([code_change/3]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

init(_) ->
    {ok, stateless}.

terminate(remove_handler, _) ->
    ok;
terminate(stop, _) ->
    ok;
terminate(Error, State) ->
    error_logger:error_report([{module, ?MODULE},
                               {self, self()},
                               {error, Error},
                               {state, State}]).

handle_event({node_advertisement, Node}, State) ->
    true = net_kernel:connect_node(Node),
    {ok, State}.

handle_info({'EXIT', _, shutdown}, _) ->
    remove_handler.

handle_call(_, _) ->
    undefined.

code_change(_, State, _) ->
    {ok, State}.
