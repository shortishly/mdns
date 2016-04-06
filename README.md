# Multicast DNS

MDNS is an implementation of the
[MDNS](http://files.multicastdns.org/draft-cheshire-dnsext-multicastdns.txt)
discovery protocol written in
[Erlang/OTP](http://www.erlang.org/). The current implementation is
sufficient for two or more Erlang nodes to self discover and form a
network.

The implementation uses the method described in
[DNS-Based Service Discovery](http://www.ietf.org/rfc/rfc6763.txt) to
register and discover nodes.

A service of type **_erlang._tcp** will be advertised by default.

```shell
avahi-browse _erlang._tcp
```

MDNS will automatically advertise its presence will discover other
**_erlang._tcp** nodes. Whether a node will advertise or discovery
other nodes is controlled by the `MDNS_CAN_ADVERTISE` and
`MDNS_CAN_DISCOVER` boolean enviroment variables.

MDNS will also automatically form an Erlang/OTP mesh network of nodes
if the boolean variable `MDNS_CAN_MESH` is true, providing the nodes
all share the same `MDNS_ENVIRONMENT` (default: "dev") and have the
same distribution cookie.

The following variables are used to control the operation of mDNS:

|application name  |environment variable   |default       |
|------------------|-----------------------|--------------|
|can\_advertise    |MDNS\_CAN\_ADVERTISE   |true          |
|can\_discover     |MDNS\_CAN\_DISCOVER    |true          |
|can\_mesh         |MDNS\_CAN\_MESH        |false         |
|environment       |MDNS\_ENVIRONMENT      |dev           |
|multicast\_address|MDNS\_MULTICAST_ADDRESS|224.0.0.251   |
|udp\_port         |MDNS\_UDP\_PORT        |5353          |
|domain            |MDNS\_DOMAIN           |.local        |
|service           |MDNS\_SERVICE          |\_erlang.\_tcp|
|ttl               |MDNS\_TTL              |120           |


MDNS uses [gproc](https://github.com/uwiger/gproc)'s
[pub/sub](https://github.com/uwiger/gproc#use-case-pubsub-patterns)
pattern. Consumers can subsribe to MDNS advertisements via
`mdns:subscribe(advertisement)`. The map accompanying the
advertisement has the following structure:

|Key     |Description                              |
|--------|-----------------------------------------|
|host    |The hostname of the advertised node      |
|node    |The node name of the advertised node     |
|port    |The advertised distribution protocol port|
|env     |The environment of this node             |

Only nodes that share the same environment can be automatically meshed
together.
