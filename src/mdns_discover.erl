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
-export([start_link/0]).
-export([stop/0]).
-export([terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


init([]) ->
    case mdns_udp:open(discover) of
        {ok, State} ->
            {ok, State};

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
    handle_record(header(Record),
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
              State) ->
    mdns_advertise:multicast(),
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
              State) ->
    case lists:member(Data, local_instances(State)) of
        true ->
            mdns_advertise:multicast(),
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

handle_record(_, msg, false, query, _, _, _, _, _, State) ->
    State.


local_instances(#{service := Service, domain := Domain}) ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    [mdns_sd:instance(Node, Hostname, Service, Domain) || {Node, _} <- Names].

handle_advertisement([#{domain := ServiceDomain,
                        type := ptr,
                        class := in,
                        ttl := TTL,
                        data := Data} | Answers],
                     Resources,
                     ServiceDomain,
                     State) ->
    Detail = maps:put(ttl, TTL, kvs([{Type,
                                      RD} || #{domain := RDomain,
                                               type := Type,
                                               data := RD} <- Resources,
                                             RDomain == Data])),
    mdns:notify(advertisement, Detail),
    handle_advertisement(Answers, Resources, ServiceDomain, State);

handle_advertisement([_ | Answers], Resources, ServiceDomain, State) ->
    handle_advertisement(Answers, Resources, ServiceDomain, State);

handle_advertisement([], _, _, State) ->
    State.

kvs(Resource) ->
    lists:foldl(
      fun
          (KV, KVS) ->
              case string:tokens(KV, "=") of
                  ["port", Port] ->
                      KVS#{port => any:to_integer(Port)};

                  ["apps", Applications] ->
                      KVS#{apps => lists:foldl(
                                     fun
                                         (Application, Apps) ->
                                             [any:to_atom(Application) | Apps]
                                     end,
                                     [],
                                     string:tokens(Applications, ","))};

                  [K, V] ->
                      KVS#{any:to_atom(K) => V}
              end
      end,
      #{},
      get_value(txt, Resource)).

get_value(Key, List) ->
    proplists:get_value(Key, List).
