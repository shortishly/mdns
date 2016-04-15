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

-module(mdns_discover).
-behaviour(gen_server).


-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([start_link/1]).
-export([stop/1]).
-export([terminate/2]).


start_link(Advertiser) ->
    gen_server:start_link(ref(Advertiser), ?MODULE, [Advertiser], []).

stop(Advertiser) ->
    gen_server:cast(ref(Advertiser), stop).


ref(Advertiser) ->
    {via, gproc, {n, l, #{module => ?MODULE,
                          service => Advertiser:service(),
                          domain => Advertiser:domain()}}}.



init([Advertiser]) ->
    case mdns_udp:open(discover) of
        {ok, State} ->
            {ok, State#{
                   advertiser => Advertiser,
                   service => Advertiser:service(),
                   domain => Advertiser:domain()}};

        {error, Reason} ->
            {stop, Reason}
    end.


handle_call(_, _, State) ->
    {stop, error, State}.


handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({udp, Socket, _, _, Packet}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, handle_packet(Packet, State)}.


terminate(_Reason, #{socket := Socket}) ->
    gen_udp:close(Socket).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_packet(Packet, #{service := Service, domain := Domain} = State) ->
    {ok, Record} = inet_dns:decode(Packet),
    Header = header(Record),
    handle_record(Header,
                  record_type(Record),
                  get_value(qr, Header),
                  get_value(opcode, Header),
                  questions(Record),
                  answers(Record),
                  authorities(Record),
                  resources(Record),
                  Service ++ Domain,
                  State).

header(Record) ->
    inet_dns:header(inet_dns:msg(Record, header)).

record_type(Record) ->
    inet_dns:record_type(Record).

questions(Record) ->
    Qs = inet_dns:msg(Record, qdlist),
    [maps:from_list(inet_dns:dns_query(Q)) || Q <- Qs].

answers(Record) ->
    rr(inet_dns:msg(Record, anlist)).

authorities(Record) ->
    rr(inet_dns:msg(Record, nslist)).

resources(Record) ->
    rr(inet_dns:msg(Record, arlist)).

rr(Resources) ->
    [maps:from_list(inet_dns:rr(Resource)) || Resource <- Resources].


handle_record(_,
              msg,
              false,
              query,
              [#{domain := ServiceDomain, type := ptr, class := in}],
              [],
              [],
              [],
              ServiceDomain,
              #{advertiser := Advertiser} = State) ->
    mdns_advertise:multicast(Advertiser),
    State;

handle_record(_,
              msg,
              false,
              query,
              [#{domain := ServiceDomain, type := ptr, class := in}],
              [#{data := Data}],
              [],
              [],
              ServiceDomain,
              #{advertiser := Advertiser} = State) ->
    case lists:member(Data, local_instances(State)) of
        true ->
            mdns_advertise:multicast(Advertiser),
            State;
        _ ->
            State
    end;

handle_record(_,
              msg,
              true,
              query,
              [],
              Answers,
              [],
              Resources,
              ServiceDomain,
              State) ->
    handle_advertisement(Answers, Resources, ServiceDomain, State);

handle_record(_, msg, false, query, _Questions, _Answers, _Authorities, _Resources, _ServiceDomain, State) ->
    State.


local_instances(#{advertiser := Advertiser}) ->
    [Instance || #{instance := Instance} <- Advertiser:instances()].

handle_advertisement([#{domain := ServiceDomain,
                        type := ptr,
                        class := in,
                        ttl := TTL,
                        data := Data} | Answers],
                     Resources,
                     ServiceDomain,
                     #{advertiser := Advertiser, service := Service} = State) ->

    %% TXT resource record contains a list of KV pairs.
    [KVS] = [RD || #{domain := RDomain,
                     type := txt,
                     data := RD} <- Resources,
                   RDomain == Data],

    %% SRV record has priority, weight, port and domain.
    [{Priority, Weight, Port, _Domain}] = [RD || #{domain := RDomain,
                                                   type := srv,
                                                   data := RD} <- Resources,
                                                 RDomain == Data],

    Detail = (txt_kvs(KVS))#{priority => Priority,
                             weight => Weight,
                             port => Port,
                             ttl => TTL,
                             instance => Data,
                             advertiser => Advertiser,
                             service => Service},
    mdns:notify(advertisement, Detail),
    handle_advertisement(Answers, Resources, ServiceDomain, State);

handle_advertisement([_Answer | Answers], Resources, ServiceDomain, State) ->
    handle_advertisement(Answers, Resources, ServiceDomain, State);

handle_advertisement([], _, _, State) ->
    State.

txt_kvs(Data) ->
    lists:foldl(
      fun
          (KV, KVS) ->
              case string:tokens(KV, "=") of
                  [K, V] ->
                      KVS#{any:to_atom(K) => V};

                  [_K] ->
                      KVS
              end
      end,
      #{},
      Data).

get_value(Key, List) ->
    proplists:get_value(Key, List).
