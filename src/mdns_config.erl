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
-export([advertise/1]).
-export([can/1]).
-export([domain/0]).
-export([environment/0]).
-export([port/1]).
-export([service/0]).
-export([ttl/0]).


can(advertise) ->
    envy:to_boolean(mdns, can_advertise, default(true));
can(discover) ->
    envy:to_boolean(mdns, can_discover, default(true));
can(mesh) ->
    envy:to_boolean(mdns, can_mesh, default(false)).

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

environment() ->
    envy:to_list(mdns, environment, default("dev")).

domain() ->
    envy:to_list(mdns, domain, default(".local")).

service() ->
    envy:to_list(mdns, service, default("_erlang._tcp")).

ttl() ->
    envy:to_integer(mdns, ttl, default(120)).


advertise(blacklist) ->
    lists:foldl(
      fun
          (Application, A) ->
              ordsets:add_element(any:to_atom(Application), A)
      end,
      ordsets:new(),
      string:tokens(envy:to_list(mdns, advertise_blacklist, default(blacklist())), ",")).

blacklist() ->
    lists:foldl(
      fun
          (Application, []) ->
              any:to_list(Application);

          (Application, A) ->
              A ++ "," ++ any:to_list(Application)
      end,
      [],
      blacklist(otp) ++ blacklist(utility)).

blacklist(otp) ->
    [compiler,
     crypto,
     eldap,
     erl_interface,
     gs,
     inets,
     jinterface,
     kernel,
     megaco,
     mnesia,
     os_mon,
     otp_mibs,
     public_key,
     runtime_tools,
     sasl,
     snmp,
     ssh,
     ssl,
     stdlib,
     syntax_tools,
     tools,
     wx,
     xmerl];

blacklist(utility) ->
    [any,
     gproc,
     envy,
     mdns,
     recon,
     sync].
    
    


default(Default) ->
    [os_env, app_env, {default, Default}].
