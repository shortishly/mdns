-module(zeroconf_node_discovery).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
         terminate/2,
	 code_change/3]).

-export([advertise/0,
	 stop/0]).


-include("zeroconf.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

advertise() ->
    gen_server:call(?SERVER, advertise).

stop() ->
    gen_server:call(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {socket}).

init(_) ->
    {ok, Socket} = gen_udp:open(?PORT, [{reuseaddr, true},
					{multicast_if, multicast_if()},
					{ip, ?ADDRESS}]),
    {ok, #state{socket = Socket}, crypto:rand_uniform(500, 1500)}.

multicast_if() ->
    {ok, Interfaces} = inet:getifaddrs(),
    multicast_if(Interfaces).

multicast_if([{_, H} | T]) ->
    case is_running_multicast_interface(proplists:get_value(flags, H)) of
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
	lists:member(multicast, Flags) andalso
	lists:member(addr, Flags).

handle_call(advertise, _, State) ->
    {reply, announce(State), State, crypto:rand_uniform(60 * 1000, 120 * 1000)};

handle_call(stop, _, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    announce(State),
    {noreply, State, crypto:rand_uniform(60 * 1000, 120 * 1000)};
handle_info({udp, _, _, _, _}, State) ->
    {noreply, State,  crypto:rand_uniform(60 * 1000, 120 * 1000)}.


terminate(_, #state{socket = Socket}) ->
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

announce(State) ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    announce(Names, Hostname, State).

announce(Names, Hostname, #state{socket = Socket}) ->
    TTL = 120,
    Message = message(Names, Hostname, TTL),
    error_logger:info_report([{socket, Socket},
			      {address, ?ADDRESS},
			      {port, ?PORT},
			      {message, Message}]),
    gen_udp:send(Socket,
		 ?ADDRESS,
		 ?PORT,
		 inet_dns:encode(Message)).

message(Names, Hostname, TTL) ->
    inet_dns:make_msg([{header, header()},
		       {anlist, answers(Names, Hostname, TTL)},
		       {arlist, resources(Names, Hostname, TTL)}]).

header() ->
    inet_dns:make_header([{id,0},
			  {qr,true},
			  {opcode,'query'},
			  {aa,true},
			  {tc,false},
			  {rd,false},
			  {ra,false},
			  {pr,false},
			  {rcode,0}]).

answers(Names, Hostname, TTL) ->
    [inet_dns:make_rr([{type, ptr},
		       {domain, ?TYPE ++ ?DOMAIN},
		       {class, in},
		       {ttl, TTL},
		       {data, zeroconf:instance(Node, Hostname)}
		      ]) || {Node, _} <- Names].

resources(Names, Hostname, TTL) ->
    services(Names, Hostname, TTL) ++ texts(Names, Hostname, TTL).

services(Names, Hostname, TTL) ->
    [inet_dns:make_rr([{domain, zeroconf:instance(Node, Hostname)},
		       {type, srv},
		       {class, in},
		       {ttl, TTL},
		       {data, {0, 0, Port, Hostname ++ ?DOMAIN}}]) || {Node, Port} <- Names].

texts(Names, Hostname, TTL) ->
    [inet_dns:make_rr([{domain, zeroconf:instance(Node, Hostname)},
		       {type, txt},
		       {class, in},
		       {ttl, TTL},
		       {data, ["node=" ++ Node,
			       "port=" ++ integer_to_list(Port)]}]) || {Node, Port} <- Names].


