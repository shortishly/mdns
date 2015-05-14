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

-module(mdns_node_discovery_event).
-export([add_handler/1,
	 add_handler/2,
	 manager/0,
	 notify_node_advertisement/1]).


manager() ->
    mdns_node_discovery_manager.

add_handler(Handler) ->
    add_handler(Handler, []).

add_handler(Handler, Args) ->
    gen_event:add_handler(manager(), Handler, Args).

notify_node_advertisement(Node) ->
    notify(manager(), {node_advertisement, Node}).

notify(Manager, Message) ->
    gen_event:notify(Manager, Message).

