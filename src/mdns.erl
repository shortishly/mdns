%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mdns).
-export([
	 start/0,
	 stop/0,
	 discovered/0,
	 make/0,
	 get_env/1
	]).

start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    gen_server:call(?MODULE, stop).

discovered() ->
    gen_server:call(?MODULE, discovered).

make() ->
    make:all([load]).

get_env(Key) ->
    gproc:get_env(l, ?MODULE, Key, [os_env, app_env]).
