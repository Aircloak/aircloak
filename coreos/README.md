CoreOS
==========

----------------------

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
- [How it works](#how-it works)
- [Interacting with the machines](#interacting-with-the-machines)
- [Using the local cluster](#using-the-local-cluster)

----------------------

# What it does

This component handles the provisioning of the CoreOS cluster. It is possible to test the cluster locally using Vagrant. See [Using the local cluster](#using-the-local-cluster) for instructions.

# What is it made up of

- Docker image for the installer: a component which installs the Air system on a CoreOS machine.
- Helper script for working with cluster machines.
- Helper script for managing the local cluster of Vagrant machines (development purposes only).

# How it works

There are various tasks required to provision cluster machines. These tasks can be fulfilled by the same machine, but for the sake of clarity we'll treat them as the distinct server roles:

- __Build server__: Builds docker images for our services and the installer
- __Docker registry__: Contains versioned docker images
- __Provisioning server__:
    * Creates the initial `cloud-config` file for CoreOS machines
    * Holds secrets (private keys, database passwords, ...) and uploads them to CoreOS machines
    * Adds/removes new machines to/from the cluster
    * Upgrades CoreOS machines

To clarify the relationship between these roles, let's analyze typical scenarios.

- __Setting up a new cluster__
    1. The process starts on the build server, which needs to have the access to the complete source of this repository. The build server will use the [package script](../package.sh) to build all images and push them to the docker registry.

    1. Next, we need to create the `cloud-config` file. This is done on the provisioning server. Using the [cluster management script](./cluster.sh), we need to invoke `REGISTRY_URL=registry_ip[:registry_port] ip_1 ip_2 ...` to create the cloud configuration.

    1. Now we can boot the machines with the given `cloud-config` file.

    1. Once the machines are booted up, the provisioning server needs to upload required secrets (keys, production-specific settings) to proper locations.

    1. If machines are properly set-up and secrets are uploaded, the installation process will start automatically. Each machine will fetch the most recent [installation docker image](./Dockerfile), and run [the setup process](./docker/install/install.sh). This will fetch all the required service images, set up systemd units, and populate etcd cluster. Once the installation finishes, the machine is ready to be used.

- __Adding a new machine to the cluster__
    1. Invoke `./cluster.sh add_machine existing_machine_ip new_machine_ip` on the provisioning server to inform the existing cluster that a new machine will be added. This must be done before the new machine is started. The output of this process is the `cloud-config` file which must be used to bootstrap the new machine.

    1. Using the `cloud-config` obtained from the previous step, boot the new machine and upload secrets to it. The machine will install itself and join to the cluster.

- __Removing the existing machine from the cluster__
    On the provisioning server invoke `./cluster.sh remove_machine existing_machine_ip machine_to_remove_ip`. This will tell the `existing_machine` to remove the `machine_to_remove` from the etcd cluster. If the machine to be removed is running, its local services will be stopped politely.

- __Upgrading a single machine__
    On the provisioning server invoke `./cluster.sh upgrade_machine machine_ip`. This will politely stop local services on the machine, then remove the existing local Air system, and restart the installation, which will cause the most recent Air system to be installed. __Note__: during the upgrade process the machine will still be a part of the etcd cluster, but it will not run any Air service.

# Interacting with the machines

Once machines are up and running, you can `ssh` to them and perform various tasks.

All services are running as `systemd` units and are prefixed with `air-*`. You can use e.g. `systemctl` and `journalctl` to manipulate these services and see their logs.

To get shells to the running containers you can invoke:

- `/aircloak/air/frontend/container.sh ssh`
- `/aircloak/air/backend/container.sh ssh`
- `/aircloak/air/router/container.sh ssh`
- `/aircloak/air/frontend/container.sh remote_console`
- `/aircloak/air/backend/container.sh remote_console`

# Using the local cluster

It is possible to setup a local cluster of Vagrant boxes. For this, you need Vagrant (at least 1.6.3) and VirtualBox.

## Setting up key-based SSH authentication

For everything to work, you need to first setup key-based SSH authentication for upcoming machines. You can invoke `./local-cluster.sh ssh_config` which will start all five machines, gather required configurations, and terminate the machines. Ultimately, the script will display the content you need to paste into your `~/.ssh/config`.

## Starting

To start the cluster you can invoke:

```
COREOS_HOST_IP=w.x.y.z ./local-cluster.sh start n
```

- `COREOS_HOST_IP` should be set to the IP address of your machine. This address will be used by the VM and Docker containers to access the Docker registry and the database on your machine.
- _n_ is the number of machines you want in the cluster (at most 5).

This will start all required local images (docker registry, database), rebuild all production images, start the machines, wait for them to be installed, and then start the local balancer.

__Network__: CoreOS and Docker containers will communicate with your own machine. For this to work, you need to either disable firewall on your machine or open necessary ports.

__OS X__: Make sure that [required ports](../osx_setup.md#port-forwarding) are forwarded to your `boot2docker` VM. The _Host IP_ value in VirtualBox settings should be left empty.

Once the machines are booted, you can `ssh` to them using their addresses: 192.168.55.101, 192.168.55.102, ... See also [Interacting with the machines](#interacting-with-the-machines).

## Adding/removing machines

To expand the cluster, you can invoke `COREOS_HOST_IP=w.x.y.z ./local-cluster.sh add_machine`. Note that if you're expanding a one-machine cluster, there will be some downtime until the second machine boots up. This is simply a property of etcd, which will not have the majority until the new etcd instance joins the cluster.

To remove a machine, you can invoke `./local-cluster.sh remove_machine` which will remove one machine from the cluster (and shut it down).

## Upgrading machines

To update a single machine, you can invoke `./local-cluster.sh upgrade_machine machine_name`, where machine name is internal Vagrant machine name (`air-01`, `air-02`, ..., `air-05`). You can see the list of running machines with `vagrant status`.

You can also perform a rolling upgrade with `./local-cluster.sh rolling_upgrade`. This will upgrade the entire cluster, one machine at a time.
