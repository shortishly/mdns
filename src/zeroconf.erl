-module(zeroconf).
-export([announce/0,
	 start/0,
	 name/0,
	 stop/0,
	 make/0]).


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
    
