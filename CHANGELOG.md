This document serves to describe breaking changes and provide upgrade hints when major changes are introduced. When you're creating a pull with some major changes, please add brief upgrade instructions here.

## Air router and CoreOS cluster

- To start required components, you can now run `./start_dependencies.sh` from the root folder. This will start dockerized etcd instances, database container, and local nginx. __OS X users__: prior to running, make sure your folders are shared on boot2docker (e.g. via calling `./osx_mount_nfs.sh folder_to_share`).
- To access the site via nginx, you need to add some entries to your `/etc/hosts`. Just watch the end of the output of `./start_dependencies.sh` for instructions.
- The `frontend` server now listens on port 8080 by default. However, you're advised to access the site via the router (https://frontend.air-local:8202). You'll need to import the certificate from `router/dev_certs/aircloak.com.chain.pem` to your browser to prevent security errors.
- There's no need to apply different etcd settings when switching from localhost to local docker containers. Different etcd instances are used for different contexts.
- There's no `etcd/config_local.sh` anymore. If you need to apply changed settings, just start the etcd container again (`etcd/container.sh start`).
- Routing rules are now a part of this repository. You can find them [here](router/docker/nginx). If you need to change some rules, do it there, rather than on the server.
- `container.sh remsh` is replaced with `container.sh ssh`
- To locally run CoreOS cluster, you'll need `nginx 1.9.3`, Vagrant (at least 1.6.3) and VirtualBox.
- OS X users: previous port forward rules for boot2docker need to be changed. Following ports need to be forwared: 4002, 4003, 4004, 5000, 5433, 8200, 8201, 10000 (see [here](README.md#exposed-container-ports) for meaning of these ports). In addition, standard ssh rule should be preserved. All other forwarding rules should be removed to avoid collisions with components running on localhost.
