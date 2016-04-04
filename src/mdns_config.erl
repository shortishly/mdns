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

-module(mdns_config).

-export([address/1]).
-export([domain/0]).
-export([port/1]).
-export([service/0]).
-export([ttl/0]).


port(udp) ->
    envy:to_integer(mdns, udp_port, default(5353)).

address(multicast) ->
    Address = envy:to_list(mdns, multicast_address, default("224.0.0.251")),
    case inet:parse_ipv4_address(Address) of
        {ok, V4} ->
            V4;

        {error, _} ->
            error(badarg, [Address])
    end.

domain() ->
    envy:to_list(mdns, domain, default(".local")).

service() ->
    envy:to_list(mdns, service, default("_erlang._tcp")).

ttl() ->
    envy:to_integer(mdns, ttl, default(120)).

default(Default) ->
    [os_env, app_env, {default, Default}].
