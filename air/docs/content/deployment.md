# Deployment

## Overview

All Aircloak provided components (Insights Air, Insights Cloak, and Insights Datasource Connector) are deployed as
docker containers. As such they can be deployed in physical as well as virtualized environments. They also lend
themselves to be deployed in managed docker environments like Kubernetes and Mesos.

For information on how many instances of the different containers are required for your setup,
please consult the [scaling](scaling.md) section of this guide.


### Configuration

All Aircloak components have a base set of configuration parameters that are statically configured. For Insights Air
this includes access credentials for its Postgres database. For Insights Cloak it includes information about which
Insights Air instance it should connect to as well as a static configuration for the datasources that should be made
available through it.

Both the Insights Air and Insights Cloak configurations contain secrets used to secure and sign communication
respectively between Insights Cloak and Insights Air, and Insights Air and the analyst.

Insights Datasource Connector derives its configuration from that of Insights Cloak.

The static configuration are stored in configuration files made available to the docker containers through the use of
[volumes](https://docs.docker.com/engine/tutorials/dockervolumes/).

## Resource requirements

### Insights Air

Insights Air has modest resource requirements. It can operate with 512MB of memory, although 1GB of memory is
recommended. For environments where latency is not a concern, it can be deployed on a host performing mixed workloads.
It is generally recommended to give Insights Air access to two or more shared CPU cores.

### Insights Cloak and Insights Datasource Connector

Insights Cloak and Insights Datasource Connectors are deployed as a single unit. Multiple Insights Cloak instances
should be deployed on separate machines.
During analysis Insights Cloak holds the data being analysed in memory. The memory requirements of Insights Cloak vary
with the size of the dataset being queried. It is recommended to deploy Insights Cloak with a minimum of 8GB of memory.
If queries routinely fail with “out of memory”-warnings the amount of memory made available should be increased.

Insights Cloak works best when given access to multiple CPU cores.

## Important notice

Docker allows restricting the amount of memory made available to a given docker container. __Do not use this form of
restriction on Insights Cloak__. It interferes with Insight Cloaks ability to detect and terminate rampant queries, and
might result in the component becoming unavailable.

If restricting the amount of memory made available to Insight Cloak is desired, it is recommended to deploy Insights
Cloak in a separate virtual machine.


