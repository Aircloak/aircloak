Air router
==========

----------------------

- [What it does](#what-it-does)
- [Running](#running)

----------------------

# What it does

This component runs an nginx web server that accepts all requests and delegates them to proper services, such as `frontend`, `api`, `backend`, and components which are not part of this repository, such as the Aircloak site (www.aircloak.com).

The routing is done for all requests, both those coming from users as well as from services (see [here](../README.md#production)). When a service wants to talk to another service, it will issue a request to the router running on the same host. Hence, from the standpoint of a single machine, the router is the single point of failure. We assume it will always be working. If that's not the case, other services on the same machine will not be able to communicate with the rest of the world. This is not dealt with in the current single-machine production. However, in the CoreOS we deal with this by making other services depending on the router. Consequently, if the router stops, other services on the same machine will stop as well.

Furthermore, the router balances requests over multiple machines in the cluster setup. Upstreams will be dynamically configured based on services registration in `etcd`. Hence, even if some service on the same host machine terminates, the router will forward to the service on some other machine in the cluster. The service container is running on the same host will always be used if it is running, which should reduce the network traffic in the normal operation mode.

# Running

To run the router locally, you need to have nginx installed. The router is started when running `./start_dependencies.sh`. If you need to (re)start just the router, you can invoke `router/run_local.sh`. If you want to start the router in foreground, you can invoke `cd router && make start`.
