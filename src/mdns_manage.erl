%% Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(mdns_manage).
-behaviour(gen_server).


-export([code_change/3]).
-export([discovered/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([start_link/0]).
-export([stop/0]).
-export([terminate/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:cast(?MODULE, stop).


discovered() ->
    gen_server:call(?MODULE, discovered).


init([]) ->
    mdns:subscribe(advertisement),
    {ok, #{discovered => #{}, cancelled => ordsets:new()}}.


handle_call(discovered, _, #{discovered := Discovered} = State) ->
    {reply, maps:keys(Discovered), State}.


handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({_, {mdns, advertisement}, #{ttl := TTL} = Advertisement}, State) ->
    {noreply,
     advertisement(
       maps:without([ttl], Advertisement),
       TTL,
       State)};

handle_info({timeout, Node}, #{discovered := Discovered,
                               cancelled := Cancelled} = State) ->
    case Discovered of
        #{Node := Timer} ->
            ok = erlang:cancel_timer(Timer, [{async, true}, {info, true}]),
            {noreply, State#{
                        discovered => maps:remove(Node, Discovered),
                        cancelled => ordsets:add_element(Timer, Cancelled)
                       }};

        #{} ->
            {noreply, State}
    end;

handle_info({cancel_timer, Timer, _}, #{cancelled := Cancelled} = State) ->
    {noreply, State#{cancelled := ordsets:del_element(Timer, Cancelled)}}.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_, _) ->
    ok.


advertisement(Advertisement, 0, #{discovered := Discovered,
                                  cancelled := Cancelled} = State) ->
    case Discovered of
        #{Advertisement := Timer} ->
            ok = erlang:cancel_timer(Timer, [{async, true}, {info, true}]),
            State#{
              discovered => maps:remove(Advertisement, Discovered),
              cancelled => ordsets:add_element(Timer, Cancelled)
             };

        #{} ->
            State
    end;

advertisement(Advertisement, TTL, State) ->
    case State of
        #{cancelled := Cancelled,
          discovered := #{Advertisement := Timer} = Discovered} ->

            ok = erlang:cancel_timer(Timer, [{async, true}, {info, true}]),
            State#{
              discovered => Discovered#{
                              Advertisement := send_after_timeout(
                                                 Advertisement, TTL)},
              cancelled => ordsets:add_element(Timer, Cancelled)
             };

        #{discovered := Discovered} ->
            State#{
              discovered => Discovered#{
                              Advertisement => send_after_timeout(
                                                 Advertisement, TTL)}}
    end.


send_after_timeout(Node, TTL) ->
    erlang:send_after(TTL * 1000, self(), {timeout, Node}).
