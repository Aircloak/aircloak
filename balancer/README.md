Air balancer
==========

----------------------

- [What it does](#what-it-does)
- [Running](#running)

----------------------

# What it does

This is a top-level component that accepts all external requests and forwards them to [routers](../router/README.md).

The component is essentially a TCP balancer, powered by the nginx 1.9 and compiled with `--with-stream` option.

# Running

Usually, you don't need to start this component manually. The component is started when you create a local CoreOS cluster via `coreos/start.sh` (see [here](../coreos/README.md)).
