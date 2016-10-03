Aircloak
========

[![Build
Status](https://travis-ci.com/Aircloak/aircloak.svg?token=SwtqZyez24jMwX5xQx9U&branch=develop)](https://magnum.travis-ci.com/Aircloak/aircloak)

This repository contains the entirety of the Aircloak system.
A system that allows databases to be queried while enforcing
the anonymity of the individuals in the dataset.

There are two main components:

- __air__: a central control point only operating on non-sensitive data
- __cloak__: a component deployed close to the data, performing the querying and anonymization

## Prerequisites

You need to have Erlang and Elixir installed. The required versions are stated in [this file](.tool-versions). The easiest way to install is to use [asdf version manager](https://github.com/asdf-vm/asdf). You need to install it, together with Erlang and Elixir plugins. Then from the project root folder run `asdf install`. Before installing, make sure you have `unixodbc` installed (__OS X developers__ see [here](./cloak/osx_erlang_with_odbc.md) for detailed instructions).

You will also need Docker 1.11 (__OS X developers__ also need [Docker for Mac](https://docs.docker.com/docker-for-mac/), see [here](./osx_docker.md) for instructions).

## Deploying

Each component can be deployed to a __deploy target__. The targets are provided in the [deploy_targets](./deploy_targets) folder.

To deploy both air and cloak, you can run `./publish.sh deploy_target`, where `deploy_target` is the name of the file from the `deploy_targets` folder (without the path). For example, `./publish.sh sasa` will deploy new versions of air and cloak to the `sasa` deploy target (which is described in `./deploy_targets/sasa`).

Deploying will always publish all __pushed__ changes from your current local branch.

You can also deploy each component separately using `./cloak/production.sh` and `./air/production.sh` scripts. Run these scripts without any argument for instructions.

### Rolling back

If something is wrong with the deployed version, you can easily rollback to a previously built version. This must be done for each component separately, using the `production.sh` helper script.

First you need to list all published versions for the given component:

```bash
# list versions
$ ./cloak/production.sh sasa versions

0.1.9 (2016-08-18T12:23:14.451047582Z)  # latest version
0.1.8 (2016-08-18T06:46:11.273391659Z)
...
```

Now, you can rollback to the previous version:

```bash
$ ./cloak/production.sh sasa rollback 0.1.8
```

### Interacting with production components

Some typical tasks you can run on a thor machine:

- getting a list of running containers - `docker ps`
- logs - `docker logs container`
- restarting a container - `docker restart container`
- stopping a container - `docker stop container && docker rm container`
- remote bash shell - `docker exec -it container /bin/bash`
- remote iex shell to cloak - `docker exec -it container bin/cloak remote_console`
- remote iex shell to air - `docker exec -it container bin/air remote_console`
