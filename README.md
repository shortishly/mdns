# Multicast DNS

mDNS is an implementation of the
[mDNS](http://files.multicastdns.org/draft-cheshire-dnsext-multicastdns.txt)
discovery protocol written in [Erlang/OTP](http://www.erlang.org/) and
enables two or more Erlang nodes to self discover and form a mesh
network.

The implementation uses the method described in
[DNS-Based Service Discovery](http://www.ietf.org/rfc/rfc6763.txt) to
register and discover services.

A service of type **_erlang._tcp** will be advertised by default. The
registered services can be viewed using `avahi-browse` as follows:

```shell
avahi-browse _erlang._tcp
```

mDNS will automatically advertise its presence and discover other
**_erlang._tcp** nodes. Whether a node will advertise or discover
other nodes is controlled by the `MDNS_CAN_ADVERTISE` and
`MDNS_CAN_DISCOVER` boolean environment variables.

mDNS will also automatically form an Erlang/OTP mesh network of nodes
if the boolean variable `MDNS_CAN_MESH` is true, providing they all
share the same `MDNS_ENVIRONMENT` (default: "dev") and have the same
distribution cookie.

For example, starting mDNS on two separate machines (in this example
`dev001.local` and `dev002.local`) reachable on the same local
network:

On `dev001.local`:

```shell
MDNS_CAN_MESH=true make shell
```

and on `dev002.local`:

```shell
MDNS_CAN_MESH=true make shell
```

After a short period both machines will have automatically formed a
mesh network:

```shell
(mdns@dev001.local)2> nodes().
['mdns@dev002.local']
```

```shell
(mdns@dev002.local)2> nodes().
['mdns@dev001.local']
```

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


mDNS uses [gproc](https://github.com/uwiger/gproc)'s
[pub/sub](https://github.com/uwiger/gproc#use-case-pubsub-patterns)
pattern. Consumers can subscribe to mDNS advertisements via
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
