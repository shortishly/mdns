-module(zeroconf_dns_sd).
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
    {ok, #state{socket = Socket}}.

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
	lists:member(multicast, Flags).

handle_call(advertise, _, State) ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    {reply, announce(Names, Hostname, State), State};

handle_call(stop, _, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, #state{socket = Socket}) ->
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


announce(Names, Hostname, #state{socket = Socket}) ->
    Domain = "_erlang._udp.local",
    Header = inet_dns:make_header([{id,0},
				   {qr,true},
				   {opcode,'query'},
				   {aa,true},
				   {tc,false},
				   {rd,false},
				   {ra,false},
				   {pr,false},
				   {rcode,0}]),
    Answer = inet_dns:make_rr([{type, ptr},
			       {domain, Domain},
			       {class, in},
			       {ttl, 4500},
			       {data, Hostname ++ "._erlang._udp.local"}
			      ]),
    Message = message(Header,
		      [Answer],
		      services(Names, Hostname, 120) ++ texts(Names, Hostname, 120)),
    error_logger:info_report([{socket, Socket},
			      {address, ?ADDRESS},
			      {port, ?PORT},
			      {message, Message}]),
    gen_udp:send(Socket,
		 ?ADDRESS,
		 ?PORT,
		 inet_dns:encode(Message)).



message(Header, Answers, Resources) ->
    inet_dns:make_msg([{header, Header},
		       {anlist, Answers},
		       {arlist, Resources}]).

services(Names, Hostname, TTL) ->
    [inet_dns:make_rr([{domain, "_erlang._udp.local"},
		       {type, srv},
		       {class, in},
		       {ttl, TTL},
		       {data, {0, 0, Port, Hostname ++ ".local"}}]) || {_, Port} <- Names].

texts(Names, _Hostname, TTL) ->
    [inet_dns:make_rr([{domain, "_erlang._udp.local"},
		       {type, txt},
		       {class, in},
		       {ttl, TTL},
		       {data, [[]]}]) || {_, _} <- Names].


