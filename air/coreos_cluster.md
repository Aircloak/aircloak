CoreOS cluster
==========

- [Servers](#servers)
- [How it works](#how-it-works)
- [Target clusters](#target-clusters)
- [The build process](#the-build-process)
- [Managing a cluster](#managing-a-cluster)

## Servers

__Common servers (used for both production and stage):__

- acdbuild.mpi-sws.org - build server
- aclb0.mpi-sws.org - load balancer
- registry.aircloak.com - Docker registry

__Production servers:__

- airdb.aircloak.com - database server
- accos01.mpi-sws.org - CoreOS machine
- accos02.mpi-sws.org - CoreOS machine
- accos03.mpi-sws.org - CoreOS machine

__Staging servers:__

- airdb-stage.aircloak.com - database server
- stage-accos01.mpi-sws.org - CoreOS machine
- stage-accos02.mpi-sws.org - CoreOS machine
- stage-accos03.mpi-sws.org - CoreOS machine

Authorized SSH keys are setup on all the machines, so you can `ssh` to them.


## How it works

The overall architecture of the cluster is as follows:

<!--- ASCII diagram made with http://asciiflow.com/ -->
```
                                   http requests
                                         +
                                         |
                                         |
                                         |
                                         |
                                   +-----v------+
          +------------------------+TCP balancer+----------------------+
          |                        +-----+------+                      |
          |                              |                             |
          |                              |                             |
          |                              |                             |
          |                              |                             |
          |                              |                             |
          |                              |                             |
+--------------------+         +--------------------+        +--------------------+
|         |          |         |         |          |        |         |          |
| +-------v--------+ |         | +-------v--------+ |        | +-------v--------+ |
| |router container| |         | |router container| |        | |router container| |
| +----------------+ |         | +----------------+ |        | +----------------+ |
|                    |         |                    |        |                    |
|                    |         |                    |        |                    |
|       +----+       |         |       +----+       |        |       +----+       |
|       |etcd<------------------------->etcd<------------------------>etcd|       |
|       +-^--+       |         |       +----+       |        |       +-^--+       |
|         |          |         |                    |        |         |          |
|         +------------------------------------------------------------+          |
|                    |         |                    |        |                    |
|     machine 1      |         |     machine 2      |        |     machine 3      |
|                    |         |                    |        |                    |
+--------------------+         +--------------------+        +--------------------+

```


On the single CoreOS machine we have the following architecture:

<!--- ASCII diagrams made with http://asciiflow.com/ -->
```
                       +----------------------+
                       |   router container   |
                       |                      |
     http(s) requests  |       +-----+        |      +------------------------+
     +------------------------->nginx+--------------->static_website container|
                       |       +^-+-^+        |      +------------------------+
                       |        | | |         |
                       +----------------------+
                                | | |
                                | | |
                                | | |
+-------------------------+     | | |     +-------------------------+
| air_frontend container  |     | | |     |  air_backend container  |
|                         |     | | |     |                         |
|         +-----+         |     | | |     |    +--------------+     |
|         |nginx<---------------+ | +---------->erlang release|     |
|         +--+--+         |       |       |    +------+-------+     |
|            |            |       |       |           |             |
|            |            |       |       |           |             |
|         +--v--+         |       |       |           |             |
|         |rails|         |       |       |           |             |
|         +--+--+         |       |       |           |             |
|            |            |       |       |           |             |
+-------------------------+       |       +-------------------------+
             |                    |                   |
             |                    |                   |
             |          +---------v--------+          |
             +---------->    local etcd    <----------+
                        +------------------+

```

On each CoreOS machine we run Docker containers as SystemD services. Starting of the required services is configured in the cloud config file which resides in `/var/lib/coreos-install/user_data`. In addition, this file contains all the required setup steps for the machine, such as authorized ssh keys, docker registry authentication info, and parameters for establishing the `etcd` cluster.


## Target clusters

Different clusters will have slightly varying configuration, so we introduced a concept of "target cluster". Currently, we have following target clusters:

- `local_vagrant` - vagrant cluster on a developer's machine
- `air_stage` - staging cluster
- `air_prod` - production cluster

It is easy to support additional clusters, for example to setup a cluster for some specific client. Each cluster is configured through a "cluster plugin". You can read more about it [here](.coreos/clusters/README.md).

For each target cluster, there is a Capistrano deploy stage with the same name.

## The build process

The build process takes place on the build server. This server contains the source code and produces Docker images. The images are then pushed to the registry server.

The build server also caches the latest built images, and in this way detects whether an image has changed since the last deploy. Only if that's the case will the new version be pushed to the registry.

The Docker registry is the authority for patch version numbers, while major/minor versions are deduced from [the version file](./VERSION) During the build the latest pushed version number is obtained from the registry, and if the image has changed, its patch version will be bumped.

The name of each Docker image is prefixed with the target cluster name. For example, the production frontend image is called `aircloak/air_prod_air_frontend`, while the stage frontend image is called `aircloak/air_stage_air_frontend`.

Moreover, we keep multiple copies of the source code on the build server - one for each target cluster. The Air source code resides in the folder `/aircloak/target_cluster/air` folder, where `target_cluster` is for example `air_stage` or `air_prod`.

All of this makes it possible to deploy from different branches to separate clusters. It also allows us to perform simultaneous deploys to different clusters.

## Managing a cluster

### Initial cluster setup

The initial setup of the cluster is currently not fully automated, but there are helper scripts which simplify this process. You need to build the latest images, prepare clean CoreOS machines, and perform the first installation. Finally, you need to configure the load balancer.


Afterwards, all updates go through the rolling upgrade process.

#### Manually building images

To manually build the latest images and push them to the Docker registry, you can run the following commands on the build server:

1. `cd` to the `/aircloak/target_cluster/air`
2. Perform `git pull` and checkout the desired branch
3. Run `CONTAINER_ENV=prod REGISTRY_URL=registry.aircloak.com IMAGE_CATEGORY=target_cluster             /aircloak/target_cluster/aircloak/air/package.sh`

In the commands above, `CONTAINER_ENV` must always be set to `prod` (indicating we're building on MPI servers), while `target_cluster` must be replaced with `air_stage` or `air_prod`.

#### Preparing CoreOS machines

This step is currently not implemented, and we're awaiting for our future administrator to support this. The general idea is to request a couple of machines which are empty CoreOS machines, with the base cloud config provided.

The base cloud config is already created by our former administrator, and it gives the build server the ssh access to the machines. It also ensures some basic monitoring is already prepared. Finally, it makes sure that each CoreOS machine is logged on to the Docker registry server. This base cloud config must reside in the `/var/lib/coreos-install/user_data` on each machine. Subsequent installation process will extend this file to create the final cloud configuration. See below for details.

It is also essential that machines have fixed static IP addresses. This is imperative for establishing an etcd cluster.

#### First installation

Assuming CoreOS machines are properly prepared, on the build server you can simply invoke :

```
REGISTRY_URL=registry.aircloak.com /aircloak/target_cluster/aircloak/air/coreos/cluster.sh \
      setup_cluster \
      target_cluster \
      machine_ip1 machine_ip2 ...
```

This will setup each machine. Once the installation is finished, the cluster is ready to be used.

The installation process is orchestrated from the build server. You can find the implementation [here](./coreos/cluster.sh). This script will upload the necessary files to the CoreOS machine, such as cluster specific `etcd` configuration, and secrets (which reside on the build server in `/aircloak/target_cluster/aircloak/air/coreos/clusters/target_cluster/secrets/` folder).

Then the installer service is started on the machine. This service is a "oneshot" SystemD unit which runs [the installation script](./coreos/docker/install/install.sh). The installation script will fetch the latest Air images, populate `etcd`, and generate the final cloud config.

Once the installer script is done, the installation process on the build server will start the system.

#### Configuring the load balancer

Once the Air system is installed you'll find the routers file which needs to be edited in the `/aircloak/target_cluster/aircloak/air/balancer/config/` folder on the balancer machine. In this file you need to list all the CoreOS machines belonging to the cluster. As soon as you save the file, the balancer will use these machines.

#### Creating the first user

You won't be able to log in if your cluster starts with a fresh database. This is because by default no user exists. To manually add a user, enter the remote console on any one of the CoreOS machines using `/aircloak/air/site/container.sh remote_console`

Now you can add the organisation and the admin login:

```
iex(1)> organisation =
          %Air.Organisation{} |>
          Air.Organisation.changeset(%{name: "Administrators"}) |>
          Air.Repo.insert!

iex(2)> organisation |>
          Ecto.build_assoc(:users) |>
          Air.User.changeset(%{
                email: "admin@aircloak.com",
                password: "admin",
                password_confirmation: "admin",
                name: "administrator"
              }) |>
          Air.Repo.insert!
```

At this point, you can login to the system as administrator and add additional users and organisations. Make sure to change the administrator password.


### Changing the cluster

#### Adding a machine to the cluster

First, you need to prepare an empty CoreOS machine (as explained [earlier](#preparing-coreos-machines)). Don't perform the Air installation step on that machine, it just need to contain the base CoreOS setup.

Now you can run the following command:

```
REGISTRY_URL=registry.aircloak.com /aircloak/target_cluster/aircloak/air/coreos/cluster.sh \
    add_machine \
    target_cluster \
    machine_from_the_cluster_ip \
    machine_to_add_ip
```

The `machine_from_the_cluster_ip` parameter should be the IP address of one (any) machine from the existing cluster, while `machine_to_add_ip` is the address of the new machine.

The command above will install the Air system on the new machine and add it to the existing `etcd` cluster.

Once the machine is added, you need to go to the load balancer server and modify the routers file, as explained earlier.

#### Removing a machine from the cluster

To remove the machine, you can simply invoke:

```
/aircloak/target_cluster/aircloak/air/coreos/cluster.sh \
    remove_machine \
    target_cluster \
    machine_from_the_cluster_ip \
    machine_to_remove_ip
```

Replace `machine_from_the_cluster_ip` with the IP of the existing cluster machine which __will not be removed__. Replace `machine_to_remove_ip` with the machine which will be removed. The machine to be removed may (but doesn't need to) be powered on. If it is powered on, its services will be stopped politely, to ensure clean termination. However, its not an error if the machine is not available. In this case, the machine will simply be removed from the `etcd` cluster.

### Upgrades

Upgrades are usually done through the [Capistrano deploy](#capistrano-deploy), and you need not worry about them. However, you may want to do it manually, for example if you want to upgrade only one machine in the cluster.

#### Upgrading a single machine

First make sure you have [rebuilt and pushed the latest images](#manually-building-images).

Then, you can invoke:

```
/aircloak/target_cluster/aircloak/air/coreos/cluster.sh \
    upgrade_machine \
    target_cluster \
    machine_to_upgrade_ip
```

Upgrade will first politely shutdown all services on the machine. Then it will remove the `/aircloak/air` folder, and all Air images. Finally, it will fetch the latest installer image and restart the installation.

During the upgrade, the machine is still a part of the etcd cluster, but none of Air services are running on it.

#### Rolling upgrade

The rolling upgrade is usually invoked via [Capistrano deploy](#capistrano-deploy), but if you want to do it manually on the build server, you can run:

```
/aircloak/target_cluster/aircloak/air/coreos/cluster.sh \
    upgrade_machine \
    target_cluster
    cluster_machine_ip
```

Where `cluster_machine_ip` is IP address of one (any) machine from the cluster.

Rolling upgrade amounts to iteratively invoking single machine upgrade on all machines in the cluster.

#### Capistrano deploy

The `cap deploy` command does the following:

- Fetches the latest code on the build server and checks out the specified branch
- Rebuilds Docker images and pushes them to the registry
- Invokes the rolling upgrade of the specified cluster

During the deploy there shouldn't be a significant downtime. However it is still possible that some operations fail during the upgrade. This can happen because we're stopping currently running services. The stoppage is done in a polite manner, but there is a timeout limit in place, so longer running computation might be terminated abruptly, possibly in the middle of processing.
