%% Copyright (c) 2012, Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

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
    true = net_kernel:connect_node(Node),
    {ok, State}.

handle_info({'EXIT', _, shutdown}, _) ->
    remove_handler.


