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

-module(mdns_advertiser).
-behaviour(gen_server).

-export([advertise/0]).
-export([multicast_if/0]).
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

advertise() ->
    gen_server:cast(?MODULE, advertise).

stop() ->
    gen_server:cast(?MODULE, stop).


init([]) ->
    case mdns_udp:open() of
        {ok, State} ->
            {ok, State#{ttl => mdns_config:ttl()}, random_timeout(initial)};

        {error, Reason} ->
            {stop, Reason}
    end.


handle_call(_, _, State) ->
    {stop, error, State}.


handle_cast(advertise, #{ttl := TTL} = State) ->
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

multicast_if() ->
    {ok, Interfaces} = inet:getifaddrs(),
    multicast_if(Interfaces).

multicast_if([{_, H} | T]) ->
    case is_running_multicast_interface(
           proplists:get_value(flags, H)) andalso
        proplists:is_defined(addr, H) of

        true ->
            v4(proplists:get_all_values(addr, H));

        false ->
            multicast_if(T)
    end.

v4([{_, _, _, _} = V4 | _]) ->
    V4;
v4([_ | T]) ->
    v4(T).

is_running_multicast_interface(Flags) ->
    lists:member(up, Flags) andalso
        lists:member(broadcast, Flags) andalso
        lists:member(running, Flags) andalso
        lists:member(multicast, Flags).

announce(State) ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    announce(Names, Hostname, State).

announce(Names,
         Hostname,
         #{address := Address, port := Port, socket := Socket} = State) ->
    Message = message(Names, Hostname, State),
    gen_udp:send(Socket, Address, Port, inet_dns:encode(Message)).

message(Names, Hostname, State) ->
    inet_dns:make_msg(
      [{header, header()},
       {anlist, answers(Names, Hostname, State)},
       {arlist, resources(Names, Hostname, State)}]).

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
        #{type := Type, domain := Domain, ttl := TTL} = State) ->
    [inet_dns:make_rr(
       [{type, ptr},
        {domain, Type ++ Domain},
        {class, in},
        {ttl, TTL},
        {data, instance(Node, Hostname, State)}]) || {Node, _} <- Names].

resources(Names, Hostname, State) ->
    services(Names, Hostname, State) ++ texts(Names, Hostname, State).

services(Names, Hostname, #{domain := Domain, ttl := TTL} = State) ->
    [inet_dns:make_rr(
       [{domain, instance(Node, Hostname, State)},
        {type, srv},
        {class, in},
        {ttl, TTL},
        {data, {0, 0, Port, Hostname ++ Domain}}]) || {Node, Port} <- Names].

texts(Names, Hostname, #{ttl := TTL} = State) ->
    [inet_dns:make_rr(
       [{domain, instance(Node, Hostname, State)},
        {type, txt},
        {class, in},
        {ttl, TTL},
        {data, ["node=" ++ Node,
                "hostname=" ++ net_adm:localhost(),
                "port=" ++ integer_to_list(Port)]}]) || {Node, Port} <- Names].

instance(Node, Hostname, #{type := Type, domain := Domain}) ->
    Node ++ "@" ++ Hostname ++ "." ++ Type ++ Domain.
