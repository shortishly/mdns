-module(zeroconf_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(zeroconf_node_discovery_server, worker),
				  {zeroconf_node_discovery_responder,
				   {gen_event, start_link, [zeroconf_node_discovery_event:manager()]},
				   permanent,
				   5000,
				   worker,
				   []},
				  ?CHILD(zeroconf_node_discovery, worker)]} }.

