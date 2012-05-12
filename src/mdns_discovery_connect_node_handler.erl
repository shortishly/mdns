-module(mdns_discovery_connect_node_handler).
-behaviour(gen_event).

-export([init/1,
	 terminate/2,
	 handle_info/2,
	 handle_event/2]).

init(_) ->
    {ok, stateless}.

terminate(remove_handler, _) ->
    ok;
terminate(stop, _) ->
    ok;
terminate(Error, State) ->
    error_logger:error_report([{module, ?MODULE},
			       {self, self()},
			       {error, Error},
			       {state, State}]).

handle_event({node_advertisement, Node}, State) ->
    net_kernel:connect_node(Node),
    {ok, State}.

handle_info({'EXIT', _, shutdown}, _) ->
    remove_handler.


