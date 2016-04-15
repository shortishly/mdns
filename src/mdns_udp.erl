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

-module(mdns_udp).

-export([open/1]).

open(discover) ->
    open(discover, mdns_config:port(udp));
open(advertise) ->
    open(advertise, 0).

open(Activity, Port) ->
    try
        Address = mdns_config:address(multicast),

        case gen_udp:open(Port, options(Activity, Address)) of
            {ok, Socket} ->
                {ok, #{address => Address, socket => Socket}};

            {error, _} = Error ->
                Error
        end

    catch _:Reason ->
            {error, Reason}
    end.


options(advertise, _Address) ->
    [binary];

options(discover, Address) ->
    [{mode, binary},
     {reuseaddr, true},
     {ip, Address},
     {multicast_ttl, 4},
     {multicast_loop, true},
     {broadcast, true},
     {add_membership, {Address, {0, 0, 0, 0}}},
     {active, once}].
