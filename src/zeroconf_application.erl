-module(zeroconf_application).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = S = zeroconf_supervisor:start_link(),
    ok = zeroconf_node_discovery_event:add_handler(zeroconf_discovery_connect_node_handler),
    S.
    


stop(_State) ->
    ok.
