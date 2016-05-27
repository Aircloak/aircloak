Air router
==========

----------------------

- [What it does](#what-it-does)
- [Running](#running)
- [Development site certificates](#development-site-certificates)

----------------------

# What it does

This component runs an nginx web server that accepts all requests and delegates them to proper services, such as `frontend`, `api`, `backend`, and components which are not part of this repository, such as the Aircloak site (www.aircloak.com).

The routing is done for all requests, both those coming from users as well as from services (see [here](../README.md#production)). When a service wants to talk to another service, it will issue a request to the router running on the same host. Hence, from the standpoint of a single machine, the router is the single point of failure. We assume it will always be working. If that's not the case, other services on the same machine will not be able to communicate with the rest of the world. This is not dealt with in the current single-machine production. However, in the CoreOS we deal with this by making other services depending on the router. Consequently, if the router stops, other services on the same machine will stop as well.

Furthermore, the router balances requests over multiple machines in the cluster setup. Upstreams will be dynamically configured based on services registration in `etcd`. Hence, even if some service on the same host machine terminates, the router will forward to the service on some other machine in the cluster. The service container is running on the same host will always be used if it is running, which should reduce the network traffic in the normal operation mode.

# Running

To run the router locally, you need to have nginx installed and available in the execution path of the non-root user. The router is started when running `./start_dependencies.sh`. If you need to (re)start just the router, you can invoke `router/run_local.sh`. If you want to start the router in foreground, you can invoke `cd router && make start`.

# Development site certificates

When a new site is added, you must recreate public key. You can use the following procedure:

1. `cd dev_cert`
1. `openssl req -new -key aircloak.com.chain.pem -out multidomain-server.csr` (when asked about Common Name, enter `aircloak.air-local`, leave other fields as default)
1. `echo 'subjectAltName=DNS:insights.air-local' > cert_extensions` (make sure to add the new site to this line, and update this readme)
1. `openssl x509 -req -in multidomain-server.csr -signkey aircloak.com.chain.pem -extfile cert_extensions -out aircloak.com.chain.crt -days 36500`
1. Replace the certificate in `aircloak.com.chain.pem` with the contents of `aircloak.com.chain.crt` (be sure to keep the key in the pem file)
1. Import the new pem, assign trust on SSL, restart the browser and verify it works.
1. Commit changed files and delete temporary files (csr, crt, and cert_extensions)
1. Add the notice in the changelog about the changed certificate to let other developers know they need to import it. Once the code is merged you could also send a notification mail to everyone.

More details can be found [here](http://aionica.computerlink.ro/2011/08/multiple-domain-selfsigned-ssltls-certificates-for-apache-namebased-ssltls-vhosts/).
