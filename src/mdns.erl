-module(mdns).
-export([start/0,
	 name/0,
	 stop/0,
	 discovered/0,
	 make/0]).

name() ->
    ?MODULE.

start() ->
    application:start(?MODULE).

stop() ->
    gen_server:call(name(), stop).

discovered() ->
    gen_server:call(name(), discovered).

make() ->
    make:all([load]).
