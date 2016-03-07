This document serves to describe breaking changes and provide upgrade hints when major changes are introduced. When you're creating a pull with some major changes, please add brief upgrade instructions here.

## CoreOS cluster in production

- The deploy commands have been slightly changed. Instead of `production` and `stage`, we now use `air_prod` and `air_stage` targets. See [here](./README.md#deploying) for more details.
- The deploy process now performs a rolling upgrade. We update one machine at the time, taking down all services, then installing the most recent versions, and starting the services.
- Images are build on the build server and pushed to the Docker registry server.
- In production, `etcd` is running directly on the CoreOS machines.

## Air router and CoreOS cluster

- You need to have `nginx`, [jq](https://stedolan.github.io/jq/), and `uuid-runtime` (provided out of the box on OS X) on your machine.
- To start required components, you can now run `./start_dependencies.sh` from the root folder. This will start dockerized etcd instances, database container, and local nginx. __OS X users__: prior to running, make sure your folders are shared on boot2docker (e.g. via calling `./osx_mount_nfs.sh folder_to_share`).
- To access the site via nginx, you need to add some entries to your `/etc/hosts`. Just watch the end of the output of `./start_dependencies.sh` for instructions.
- The `frontend` rails server now listens on port 20024 by default. However, you're advised to access the site via the router (https://frontend.air-local:20000). You'll need to import the certificate from `router/dev_certs/aircloak.com.chain.pem` to your browser to prevent security errors.
- You don't need to run airpub separately. It is started with the backend system.
- Ports used for etcd and database have changed. Local etcd listens on ports 20020 (local dev), 20120 (dockerized components), and 20220 (tests), while database server listens on port 20002.
- There's no need to apply different etcd settings when switching from localhost to local docker containers. Different etcd instances are used for different contexts.
- There's no `etcd/config_local.sh` anymore. If you need to apply changed settings, just start the etcd container again (`etcd/container.sh start`).
- Routing rules are now a part of this repository. You can find them [here](router/docker/nginx). If you need to change some rules, do it there, rather than on the server.
- `container.sh remsh` is replaced with `container.sh ssh`
- Semi-automatic versioning scheme is introduced as described [here](./README.md#versioning).
- To locally run CoreOS cluster, you'll need Vagrant (at least 1.6.3) and VirtualBox.
- OS X users: previous port forward rules for boot2docker need to be changed. The complete list of forwarded ports can be seen [here](./osx_setup.md#port-forwarding). In addition, standard ssh forwarding rule, as setup by boot2docker, should be preserved.
