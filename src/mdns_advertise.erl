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

-module(mdns_advertise).
-behaviour(gen_server).


-export([multicast/0]).
-export([start_link/0]).

-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([stop/0]).
-export([terminate/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

multicast() ->
    gen_server:cast(?MODULE, multicast).

stop() ->
    gen_server:cast(?MODULE, stop).


init([]) ->
    case mdns_udp:open(advertise) of
        {ok, State} ->
            {ok, State#{
                   environment => mdns_config:environment(),
                   ttl => mdns_config:ttl()},
             random_timeout(initial)};

        {error, Reason} ->
            {stop, Reason}
    end.


handle_call(_, _, State) ->
    {stop, error, State}.

handle_cast(multicast, #{ttl := TTL} = State) ->
    case announce(State) of
        ok ->
            {noreply, State, random_timeout(announcements, TTL)};

        {error, _} = Error ->
            {stop, Error, State}
    end;
handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info(timeout, #{ttl := TTL} = State) ->
    case announce(State) of
        ok ->
            {noreply, State, random_timeout(announcements, TTL)};

        {error, _} = Error ->
            {stop, Error, State}
    end;
handle_info({udp, _, _, _, _}, #{ttl := TTL} = State) ->
    {noreply, State,  random_timeout(announcements, TTL)}.


terminate(_, #{socket := Socket} = State) ->
    announce(State#{ttl => 0}),
    gen_udp:close(Socket).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


random_timeout(initial) ->
    crypto:rand_uniform(500, 1500).

random_timeout(announcements, TTL) ->
    crypto:rand_uniform(TTL * 500, TTL * 1000).


announce(#{address := Address, socket := Socket} = State) ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    gen_udp:send(Socket, Address, mdns_config:port(udp), message(Names, Hostname, State)).


message(Names, Hostname, State) ->
    inet_dns:encode(
      inet_dns:make_msg(
        [{header, header()},
         {anlist, answers(Names, Hostname, State)},
         {arlist, resources(Names, Hostname, State)}])).


header() ->
    inet_dns:make_header(
      [{id, 0},
       {qr, true},
       {opcode, query},
       {aa, true},
       {tc, false},
       {rd, false},
       {ra, false},
       {pr, false},
       {rcode, 0}]).


answers(Names,
        Hostname,
        #{service := Service, domain := Domain, ttl := TTL}) ->
    [inet_dns:make_rr(
       [{type, ptr},
        {domain, Service ++ Domain},
        {class, in},
        {ttl, TTL},
        {data, mdns_sd:instance(
                 Node, Hostname, Service, Domain)}]) || {Node, _} <- Names].


resources(Names, Hostname, State) ->
    services(Names, Hostname, State) ++ texts(Names, Hostname, State).


services(Names, Hostname, #{service := Service, domain := Domain,
                            ttl := TTL}) ->
    [inet_dns:make_rr(
       [{domain, mdns_sd:instance(Node, Hostname, Service, Domain)},
        {type, srv},
        {class, in},
        {ttl, TTL},
        {data, {0, 0, Port, Hostname ++ Domain}}]) || {Node, Port} <- Names].


texts(Names, Hostname, #{service := Service,
                         domain := Domain,
                         ttl := TTL,
                         environment := Environment}) ->
    [inet_dns:make_rr(
       [{domain, mdns_sd:instance(Node, Hostname, Service, Domain)},
        {type, txt},
        {class, in},
        {ttl, TTL},
        {data, kvs(Node, Port, Environment)}]) || {Node, Port} <- Names].

kvs(Node, Port, Environment) ->
    [kv("node", Node),
     kv("host", net_adm:localhost()),
     kv("env", Environment),
     kv("port", Port)].

kv(Key, Value) when length(Key) =< 9 ->
    Key ++ "=" ++ any:to_list(Value).
