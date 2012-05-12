-module(zeroconf_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, T), {I, {I, start_link, []}, permanent, 5000, T, [I]}).
-define(CHILD(I, T, P), {I, {I, start_link, P}, permanent, 5000, T, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link([]).

start_link(Parameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Parameters).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Parameters) ->
    {ok, { {one_for_one, 5, 10}, children(Parameters)} }.

children(Parameters) ->
    [node_discovery_server_spec(Parameters),
     node_discovery_responder_spec(),
     node_discovery_spec(Parameters)].

node_discovery_server_spec(Parameters) ->
    ?CHILD(zeroconf_node_discovery_server, worker, Parameters).

node_discovery_spec(Parameters) ->
    ?CHILD(zeroconf_node_discovery, worker, Parameters).

node_discovery_responder_spec() ->
    {zeroconf_node_discovery_responder, {
       gen_event,
       start_link, [
		    {local, zeroconf_node_discovery_event:manager()}
		   ]},
     permanent,
     5000,
     worker,
     []}.


