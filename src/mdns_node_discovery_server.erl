%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(mdns_node_discovery_server).
-behaviour(gen_server).
-define(SERVER, {via, gproc, {n, l, ?MODULE}}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	 start_link/0,
	 start_link/1,
	 stop/0
	]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
	 init/1,
	 handle_call/3,
	 handle_info/2,
         terminate/2,
	 code_change/3,
	 handle_cast/2
	]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    start_link([]).

start_link(Parameters) ->
    gen_server:start_link(?SERVER, ?MODULE, Parameters, []).

stop() ->
    gen_sever:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    lists:foreach(fun(Parameter) -> self() ! {Parameter, mdns:get_env(Parameter)} end, [port, address, domain, type]),
    self() ! connect,
    {ok, #{discovered => []}}.

handle_call(discovered, _, #{discovered := Discovered} = State) ->
    {reply, Discovered, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({address, Address}, State) ->
    case inet:parse_ipv4_address(Address) of
	{ok, IPv4} ->
	    {noreply, State#{address => IPv4}};
	{error, _} = Error ->
	    {stop, Error, State}
    end;
handle_info({domain, Domain}, State) ->
    {noreply, State#{domain => Domain}};
handle_info({port, Port}, State) ->
    {noreply, State#{port => list_to_integer(Port)}};
handle_info({type, Type}, State) ->
    {noreply, State#{type => Type}};
handle_info(connect, #{address := Address, port := Port} = State) ->
    {ok, Socket} = gen_udp:open(Port, [{mode, binary},
					{reuseaddr, true},
					{ip, Address},
					{multicast_ttl, 4},
					{multicast_loop, true},
					{broadcast, true},
					{add_membership, {Address, {0, 0, 0, 0}}},
					{active, once}]),
    ok = net_kernel:monitor_nodes(true),
    {noreply, State#{socket => Socket}};
handle_info({nodeup, _}, State) ->
    {noreply, State};
handle_info({nodedown, Node}, #{discovered := Discovered} = State) ->
    {noreply, State#{discovered := lists:delete(Node, Discovered)}};
handle_info({udp, Socket, _, _, Packet}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, handle_packet(Packet, State)}.


terminate(_Reason, #{socket := Socket}) ->
    net_kernel:monitor_nodes(false),
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_packet(Packet, State) ->
    {ok, Record} = inet_dns:decode(Packet),
    handle_record(Record, State).

handle_record(Record, State) ->
    Header = header(Record),
    handle_record(Header, record_type(Record), get_value(qr, Header), get_value(opcode, Header), questions(Record), answers(Record), authorities(Record), resources(Record), State).
    
header(Record) ->
    inet_dns:header(inet_dns:msg(Record, header)).

record_type(Record) ->
    inet_dns:record_type(Record).

questions(Record) ->
    [inet_dns:dns_query(Query) || Query <- inet_dns:msg(Record, qdlist)].

answers(Record) ->
    [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, anlist)].
		   
authorities(Record) ->
    [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, nslist)].

resources(Record) ->
    [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, arlist)].




handle_record(_, msg, false, query, [Question], [], [], [], State) ->
    case {type_domain(State), domain_type_class(Question)} of
	{TypeDomain, {TypeDomain, ptr, in}} ->
	    mdns_node_discovery:advertise(),
	    State;
	_ ->
	    State
    end;
handle_record(_, msg, false, query, [Question], [Answer], [], [], State) ->
    case {type_domain(State), domain_type_class(Question)} of
	{TypeDomain, {TypeDomain, ptr, in}} ->
	    case lists:member(data(Answer), local_instances(State)) of
		true ->
		    mdns_node_discovery:advertise(),
		    State;
		_ ->
		    State
	    end;
	_ ->
	    State
    end;
handle_record(_, msg, true, query, [], Answers, [], Resources, State) ->
    handle_advertisement(Answers, Resources, State);

handle_record(_, msg, false, query, _, _, _, _, State) ->
    State.

local_instances(State) ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    [instance(Node, Hostname, State) || {Node, _} <- Names].

instance(Node, Hostname, #{type := Type, domain := Domain}) ->
    Node ++ "@" ++ Hostname ++ "." ++ Type ++ Domain.

handle_advertisement([Answer | Answers], Resources, #{discovered := Discovered} = State) ->
    case {type_domain(State), domain_type_class(Answer), ttl(Answer)} of
	{TypeDomain, {TypeDomain, ptr, in}, 0} ->
	    Node = node_and_hostname([{type(Resource), data(Resource)} || Resource <- Resources, domain(Resource) =:= data(Answer)]),
	    handle_advertisement(Answers, Resources, State#{discovered := lists:delete(Node, Discovered)});

	{TypeDomain, {TypeDomain, ptr, in}, TTL} when TTL > 0 ->
	    Node = node_and_hostname([{type(Resource), data(Resource)} || Resource <- Resources,
									  domain(Resource) =:= data(Answer)]),
	    
	    case lists:member(Node, Discovered) of
		false when node() =/= Node ->
		    mdns_node_discovery_event:notify_node_advertisement(Node),
		    mdns_node_discovery:advertise(),
		    handle_advertisement(Answers, Resources, State#{discovered := [Node | Discovered]});
		
		_ ->
		    handle_advertisement(Answers, Resources, State)
	    end;
	_ ->
	    handle_advertisement(Answers, Resources, State)
    end;
handle_advertisement([], _, State) ->
    State.


node_and_hostname(P) ->
    list_to_atom(node_name(get_value(txt, P)) ++ "@" ++ host_name(get_value(txt, P))).

node_name([[$n, $o, $d, $e, $= | Name] | _]) ->
    Name;
node_name([_ | T]) ->
    node_name(T).

host_name([[$h, $o, $s, $t, $n, $a, $m, $e, $= | Hostname] | _]) ->
    Hostname;
host_name([_ | T]) ->
    host_name(T).

		

type_domain(#{type := Type, domain := Domain}) ->
    Type ++ Domain.

domain_type_class(Resource) ->
    {domain(Resource), type(Resource), class(Resource)}.


domain(Resource) ->
    get_value(domain, Resource).

type(Resource) ->
    get_value(type, Resource).

class(Resource) ->
    get_value(class, Resource).

data(Resource) ->
    get_value(data, Resource).
	
ttl(Resource) ->		    
    get_value(ttl, Resource).
		    
get_value(Key, List) ->
    proplists:get_value(Key, List).
