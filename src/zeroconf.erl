-module(zeroconf).
-export([announce/0,
	 start/0,
	 name/0,
	 stop/0,
	 make/0,
	 instance/2]).

-include("zeroconf.hrl").

name() ->
    ?MODULE.

start() ->
    application:start(?MODULE).

announce() ->
    gen_server:call(name(), announce).

stop() ->
    gen_server:call(name(), stop).

make() ->
    make:all([load]).
    
instance(Node, Hostname) ->
    Node ++ "@" ++ Hostname ++ "." ++ ?TYPE ++ ?DOMAIN.
