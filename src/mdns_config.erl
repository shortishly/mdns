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
-export([can/1]).
-export([domain/0]).
-export([environment/0]).
-export([port/1]).
-export([service/0]).
-export([ttl/0]).


can(advertise) ->
    envy(to_boolean, can_advertise, true);
can(discover) ->
    envy(to_boolean, can_discover, true);
can(mesh) ->
    envy(to_boolean, can_mesh, false).

port(udp) ->
    envy(to_integer, udp_port, 5353).

address(multicast) ->
    Address = envy(to_list, multicast_address, "224.0.0.251"),
    case inet:parse_ipv4_address(Address) of
        {ok, V4} ->
            V4;

        {error, _} ->
            error(badarg, [Address])
    end.

environment() ->
    envy(to_list, environment, "dev").

domain() ->
    envy(to_list, domain, ".local").

service() ->
    envy(to_list, service, "_erlang._tcp").

ttl() ->
    envy(to_integer, ttl, 120).

envy(To, Name, Default) ->
    envy:To(mdns, Name, default(Default)).

default(Default) ->
    [os_env, app_env, {default, Default}].
