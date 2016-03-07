CoreOS
==========

----------------------

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
- [Using the local cluster](#using-the-local-cluster)

----------------------

# What it does

This component handles the provisioning of the CoreOS cluster. You can find more details about working with clusters [here](../coreos_cluster.md). This document describes how to work with the local Vagrant cluster.


# What is it made up of

- Docker image for the installer: a component which installs the Air system on a CoreOS machine.
- Helper script for working with cluster machines.
- Helper script for managing the local cluster of Vagrant machines (development purposes only).

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

This will start all required local images (docker registry, database), rebuild all Air images, start the machines, wait for them to be installed, and then start the local balancer.

__Network__: CoreOS and Docker containers will communicate with your own machine. For this to work, you need to either disable firewall on your machine or open necessary ports.

__OS X__: Make sure that [required ports](../osx_setup.md#port-forwarding) are forwarded to your `boot2docker` VM. The _Host IP_ value in VirtualBox settings should be left empty.

Once the machines are booted, you can `ssh` to them using their addresses: 192.168.55.101, 192.168.55.102, ...

## Adding/removing machines

To expand the cluster, you can invoke `COREOS_HOST_IP=w.x.y.z ./local-cluster.sh add_machine`. Note that if you're expanding a one-machine cluster, there will be some downtime until the second machine boots up. This is simply a property of etcd, which will not have the majority until the new etcd instance joins the cluster.

To remove a machine, you can invoke `./local-cluster.sh remove_machine` which will remove one machine from the cluster (and shut it down).

## Upgrading machines

To update a single machine, you can invoke `./local-cluster.sh upgrade_machine machine_name`, where machine name is internal Vagrant machine name (`air-01`, `air-02`, ..., `air-05`). You can see the list of running machines with `vagrant status`.

You can also perform a rolling upgrade with `./local-cluster.sh rolling_upgrade`. This will upgrade the entire cluster, one machine at a time.
