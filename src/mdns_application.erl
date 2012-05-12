-module(mdns_application).
-behaviour(application).

%% Application callbacks
-export([start/2,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = S = mdns_supervisor:start_link([application:get_all_env()]),
    ok = mdns_node_discovery_event:add_handler(mdns_discovery_connect_node_handler),
    S.
    


stop(_State) ->
    ok.
