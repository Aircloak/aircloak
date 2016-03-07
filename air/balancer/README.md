Air balancer
==========

----------------------

- [What it does](#what-it-does)
- [Running](#running)

----------------------

# What it does

This is a top-level component that accepts all external requests and forwards them to [routers](../router/README.md).

The component is essentially a TCP balancer, powered by HAProxy 1.5. We use haproxy to properly pass IP address of the real client. The balancer use the [Proxy protocol](http://www.haproxy.org/download/1.5/doc/proxy-protocol.txt) to include the IP address of the client. On the receiver side, the router (nginx) uses [http_real_ip](http://nginx.org/en/docs/http/ngx_http_realip_module.html) module, together with `proxy_protocol` listen option, to use proper receiver address.

# Running

Usually, you don't need to start this component manually. The component is started when you create a local CoreOS cluster via `coreos/start.sh` (see [here](../coreos/README.md)).
