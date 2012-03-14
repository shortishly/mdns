-module(zeroconf_node_discovery_server).
-behaviour(gen_server).
-import(proplists, [get_value/2]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
	 handle_call/3,
	 handle_info/2,
         terminate/2,
	 code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, zeroconf:name()}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {socket, discovered=[]}).
-include("zeroconf.hrl").

init(_) ->
    {ok, Socket} = gen_udp:open(?PORT, [{mode, binary},
					{reuseaddr, true},
					{ip, ?ADDRESS},
					{multicast_ttl, 4},
					{multicast_loop, true},
					{broadcast, true},
					{add_membership, {?ADDRESS, {0, 0, 0, 0}}},
					{active, once}]),
    ok = net_kernel:monitor_nodes(true),
    {ok, #state{socket = Socket}}.

handle_call(stop, _, State) ->
    {stop, normal, State}.

handle_info({nodeup, _}, State) ->
    {noreply, State};
handle_info({nodedown, Node}, #state{discovered = Discovered} = State) ->
    {noreply, State#state{discovered = lists:delete(Node, Discovered)}};
handle_info({udp, Socket, IP, InPortNo, Packet}, S1) ->
    {ok, Record} = inet_dns:decode(Packet),
    Header = inet_dns:header(inet_dns:msg(Record, header)),
    Type = inet_dns:record_type(Record),
    Questions = [inet_dns:dns_query(Query) || Query <- inet_dns:msg(Record, qdlist)],
    Answers = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, anlist)],
    Authorities = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, nslist)],
    Resources = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, arlist)],
    error_logger:info_report([
			      {ip, IP},
			      {source_port, InPortNo},
			      {record, Record},
			      {header, Header},
			      {record_type, Type},
			      {questions, Questions},
			      {answers, Answers},
			      {authorities, Authorities},
			      {resources, Resources}
			     ]),
    S2 = handle_record(Header,
		       Type,
		       get_value(qr, Header),
		       get_value(opcode, Header),
		       Questions,
		       Answers,
		       Authorities,
		       Resources,
		       S1),
    inet:setopts(Socket, [{active, once}]),
    {noreply, S2}.

terminate(_Reason, #state{socket = Socket}) ->
    net_kernel:monitor_nodes(false),
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_record(_, msg, false, 'query', [Question], [], [], [], State) ->
    case domain_type_class(Question) of
	{?TYPE ++ ?DOMAIN, ptr, in} ->
	    zeroconf_dns_sd:advertise(),
	    State;
	_ ->
	    State
    end;
handle_record(_, msg, false, 'query', [Question], [Answer], [], [], State) ->
    case domain_type_class(Question) of
	{?TYPE ++ ?DOMAIN, ptr, in} ->
	    case lists:member(data(Answer), local_instances()) of
		true ->
		    zeroconf_dns_sd:advertise(),
		    State;
		_ ->
		    State
	    end;
	_ ->
	    State
    end;
handle_record(_, msg, true, 'query', [], Answers, [], Resources, State) ->
    handle_advertisement(Answers, Resources, State).


local_instances() ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    [zeroconf:instance(Node, Hostname) || {Node, _} <- Names].

handle_advertisement([Answer | Answers], Resources, #state{discovered = Discovered} = State) ->
    case domain_type_class(Answer) of
	{?TYPE ++ ?DOMAIN, ptr, in} ->
	    case lists:member(data(Answer), local_instances()) of
		false ->
		    Node = node_and_hostname([{type(Resource), data(Resource)} || Resource <- Resources,
										  domain(Resource) =:= data(Answer)]),
		    case lists:member(Node, Discovered) of
			false ->
			    error_logger:info_report([{module, ?MODULE},
						      {discovered, Node}]),
			    zeroconf_node_discovery_event:notify_node_advertisement(Node),
			    zeroconf_node_discovery:advertise(),
			    State#state{discovered = [Node | Discovered]};

			true ->
			    State
		    end;

		_ ->
		    handle_advertisement(Answers, Resources, State)
	    end;
	_ ->
	    handle_advertisement(Answers, Resources, State)
    end;
handle_advertisement([], _, State) ->
    State.


node_and_hostname(P) ->
    {_, _, _, Hostname} = get_value(srv, P),
    list_to_atom(node_name(get_value(txt, P)) ++ "@" ++ Hostname).

node_name([[$n, $o, $d, $e, $= | Name], _]) ->
    Name;
node_name([_ | T]) ->
    node_name(T).

    





		    
		

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
	
		    
		    
    
    

