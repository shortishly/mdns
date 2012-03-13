-module(zeroconf_application).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    zeroconf_supervisor:start_link().

stop(_State) ->
    ok.
