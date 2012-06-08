Erlang MDNS
===========

Erlang MDNS [dl] -- 2012.
[dl]: https://github.com/shortishly/erlang-mdns.git

Introduction
------------

Erlang MDNS is an implementation of the [MDNS][cheshire] discovery
protocol written in [Erlang][erlang]. The current implementation is
sufficient for two or more Erlang nodes to self discover and form a
network.

[cheshire]: http://files.multicastdns.org/draft-cheshire-dnsext-multicastdns.txt
[erlang]: http://www.erlang.org/


Quick Start
-----------

Start a MDNS browser (at least on Mac OS X, other OS check local
documentation for similar) using:

(mdns home)/browse.sh

In a separate terminal start the server:

(mdns home)/start.sh

You should see a service type of **_erlang._tcp** being advertised in
the MDNS browser. Quitting the server [using q()] should also result
in the service being removed from the browser.

