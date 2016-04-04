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

The following variables are used to control the operation of mDNS:

|application name  |environment variable   |default       |
|------------------|-----------------------|--------------|
|multicast\_address|MDNS\_MULTICAST_ADDRESS|224.0.0.251   |
|udp\_port         |MDNS\_UDP\_PORT        |5353          |
|domain            |MDNS\_DOMAIN           |.local        |
|service           |MDNS\_SERVICE          |\_erlang.\_tcp|
|ttl               |MDNS\_TTL              |120           |
