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

-module(mdns_erlang_tcp_advertiser).


-export([domain/0]).
-export([instances/0]).
-export([service/0]).

domain() ->
    ".local".

service() ->
    "_erlang._tcp".

instances() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, Names} = net_adm:names(),
    [#{hostname => Hostname,
       port => Port,
       instance => instance(Name, Hostname, service(), domain()),
       kvs => #{host => net_adm:localhost(),
                env => mdns_config:environment(),
                node => Name,
                apps => apps(),
                vsn => mdns:vsn()},
       priority => 123,
       weight => 321} || {Name, Port} <- Names].


instance(Node, Hostname, Service, Domain) ->
    Node ++ "@" ++ Hostname ++ "." ++ Service ++ Domain.


apps() ->
    apps(mdns_config:advertise(whitelist), mdns_config:advertise(blacklist)).


apps([], Blacklist) ->
    lists:foldl(
      fun
          ({Application, _, _}, [] = A) ->
              case ordsets:is_element(Application, Blacklist) of
                  true ->
                      A;

                  false ->
                      any:to_list(Application)
              end;

          ({Application, _, _}, A) ->
              case ordsets:is_element(Application, Blacklist) of
                  true ->
                      A;
                  false ->
                      %% comma separated list of applications that are
                      %% not blacklisted
                      A ++ "," ++ any:to_list(Application)
              end
      end,
      [],
      application:which_applications());

apps(Whitelist, _) ->
    lists:foldl(
      fun
          ({Application, _, _}, [] = A) ->
              case ordsets:is_element(Application, Whitelist) of
                  true ->
                      any:to_list(Application);

                  false ->
                      A
              end;

          ({Application, _, _}, A) ->
              case ordsets:is_element(Application, Whitelist) of
                  true ->
                      %% comma separated list of white listed
                      %% applications
                      A ++ "," ++ any:to_list(Application);

                  false ->
                      A
              end
      end,
      [],
      application:which_applications()).


