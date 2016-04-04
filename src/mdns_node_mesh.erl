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

-module(mdns_node_mesh).
-behaviour(gen_server).

-export([start_link/0]).
-export([stop/0]).

-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


init([]) ->
    ok = net_kernel:monitor_nodes(true),
    mdns:subscribe(advertisement),
    {ok, #{discovered => []}}.


handle_call(_, _, State) ->
    {stop, error, State}.


handle_cast(stop, State) ->
    {stop, error, State}.


handle_info({_, {mdns, advertisement}, #{node := Node}}, State) ->
    true = net_kernel:connect_node(Node),
    {noreply, State};
handle_info({nodeup, _}, State) ->
    {noreply, State};
handle_info({nodedown, Node}, #{discovered := Discovered} = State) ->
    {noreply, State#{discovered := lists:delete(Node, Discovered)}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    net_kernel:monitor_nodes(false).
