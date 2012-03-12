-module(zeroconf_dns_server).
-behaviour(gen_server).

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

-record(state, {socket}).
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
    {ok, #state{socket = Socket}}.

handle_call(stop, _, State) ->
    {stop, normal, State}.

handle_info({udp, Socket, IP, InPortNo, Packet}, State) ->
    {ok, Record} = inet_dns:decode(Packet),
    error_logger:info_report([
			      {ip, IP},
			      {source_port, InPortNo},
			      {record, Record},
			      {header, inet_dns:header(inet_dns:msg(Record, header))},
			      {record_type, inet_dns:record_type(Record)},
			      {questions, [inet_dns:dns_query(Query) || Query <- inet_dns:msg(Record, qdlist)]},
			      {answers, [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, anlist)]},
			      {authorities, [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, nslist)]},
			      {resources, [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, arlist)]}
			     ]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

