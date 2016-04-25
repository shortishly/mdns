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


-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([multicast/1]).
-export([start_link/1]).
-export([stop/1]).
-export([terminate/2]).


start_link(Advertiser) ->
    gen_server:start_link(ref(Advertiser), ?MODULE, [Advertiser], []).

multicast(Advertiser) ->
    gen_server:cast(ref(Advertiser), multicast).

stop(Advertiser) ->
    gen_server:cast(ref(Advertiser), stop).


ref(Advertiser) ->
    {via, gproc, {n, l, #{module => ?MODULE, service => Advertiser:service()}}}.

init([Advertiser]) ->
    case mdns_udp:open(advertise) of
        {ok, State} ->
            {ok, State#{
                   advertiser => Advertiser,
                   domain => mdns_config:domain(),
                   service => Advertiser:service(),
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
    _ = announce(State#{ttl => 0}),
    gen_udp:close(Socket).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


random_timeout(initial) ->
    crypto:rand_uniform(500, 1500).

random_timeout(announcements, TTL) ->
    crypto:rand_uniform(TTL * 500, TTL * 1000).


announce(#{address := Address,
           advertiser := Advertiser,
           socket := Socket} = State) ->
    Instances = Advertiser:instances(),
    gen_udp:send(
      Socket,
      Address,
      mdns_config:port(udp),
      inet_dns:encode(
        inet_dns:make_msg(
          [{header, header()},
           {anlist, answers(Instances, State)},
           {arlist, resources(Instances, State)}]))).

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


answers(Instances, #{domain := Domain, service := Service, ttl := TTL}) ->
    [inet_dns:make_rr(
       [{type, ptr},
        {domain, Service ++ Domain},
        {class, in},
        {ttl, TTL},
        {data, Instance}]) || #{instance := Instance} <- Instances].


resources(Instances, State) ->
    services(Instances, State) ++ texts(Instances, State).


services(Instances, #{domain := Domain, ttl := TTL}) ->
    [inet_dns:make_rr(
       [{domain, Instance},
        {type, srv},
        {class, in},
        {ttl, TTL},
        {data, {Priority, Weight, Port, Hostname ++ Domain}}]) || #{
                       instance := Instance,
                       priority := Priority,
                       weight := Weight,
                       port := Port,
                       hostname := Hostname} <- Instances].


texts(Instances, #{ttl := TTL}) ->
    [inet_dns:make_rr(
       [{domain, Instance},
        {type, txt},
        {class, in},
        {ttl, TTL},
        {data, kvs(KVS)}]) || #{instance := Instance, properties := KVS} <- Instances].

kvs(KVS) ->
    maps:fold(
      fun
          (Key, Value, A) ->
              [kv(Key, Value) | A]
      end,
      [],
      KVS).

kv(Key, Value) when is_list(Key) andalso length(Key) =< 9 ->
    Key ++ "=" ++ any:to_list(Value);
kv(Key, Value) when not(is_list(Key)) ->
    kv(any:to_list(Key), Value).
