-module(zeroconf_node_discovery_event).
-export([add_handler/1,
	 add_handler/2,
	 manager/0,
	 notify_advertisement/1]).


manager() ->
    {local, zeroconf_node_discovery_manager}.

add_handler(Handler) ->
    add_handler(Handler, []).

add_handler(Handler, Args) ->
    gen_event:add_handler(manager(), Handler, Args).

notify_advertisement(Node) ->
    notify(manager(), {advertisement, Node}).

notify(Manager, Message) ->
    gen_event:notify(Manager, Message).

