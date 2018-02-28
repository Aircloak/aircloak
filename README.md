dummy change 2

Aircloak
========

This repository contains the entirety of the Aircloak system.
A system that allows databases to be queried while enforcing
the anonymity of the individuals in the dataset.

There are two main components:

- __air__: a central control point only operating on non-sensitive data
- __cloak__: a component deployed close to the data, performing the querying and anonymization

## Prerequisites

You need to have Erlang, Elixir, and NodeJS installed. The required versions are stated in [this file](.tool-versions).
Please use the [asdf version manager](https://github.com/asdf-vm/asdf) to install the correct versions. `asdf` is also
used on CI as well as when building docker containers.
This way you can ensure the version you are using locally is the same as the one being used in production and during testing.
You need to install [asdf](https://github.com/asdf-vm/asdf), together with the [Erlang](https://github.com/asdf-vm/asdf-erlang),
[Elixir](https://github.com/asdf-vm/asdf-elixir),
and [NodeJS](https://github.com/asdf-vm/asdf-nodejs) plugins.

Before installing erlang, make sure you have `unixodbc` installed (__macOS developers__ see [here](./cloak/osx_erlang_with_odbc.md) for detailed instructions).
Once `asdf` and the required plugins are installed, run `asdf install` from the root folder of the project.

You will also need Docker 1.11 (__macOS developers__ also need [Docker for Mac](https://docs.docker.com/docker-for-mac/), see [here](./macos_docker.md) for instructions).

You will need to install [yarn](https://yarnpkg.com/en/docs/install) on your development machine as well.
It replaces the node package manager. Using `yarn` we ensure we are using the same JavaScript packages
across our individual development machines.

## Deploying

Each component can be deployed to a __deploy target__. The targets are provided in the [deploy_targets](./deploy_targets) folder.

### From branch

To deploy both `air` and `cloak` from a branch (for example `master`), you can run `./publish.sh deploy_target`, where `deploy_target` is the name of the file from the `deploy_targets` folder (without the path). For example, `./publish.sh sasa` will deploy new versions of `air` and `cloak` to the `sasa` deploy target (which is described in `./deploy_targets/sasa`).

Deploying will always publish all __pushed__ changes from your current local branch.

You can also deploy each component separately using `./cloak/production.sh` and `./air/production.sh` scripts. Run these scripts without any argument for instructions.

### Running a previously built image

It is possible to start your containers with a previously built image. This can be useful if you want to test the behaviour of a previous version without needing to rebuild the image.

For example, to start your system with the version `17.3.0`, you can run the following commands:

```bash
# from the cloak folder
./production.sh deploy_target start_at_version aircloak/cloak:17.3.0

# from the air folder
./production.sh deploy_target start_at_version aircloak/air:17.3.0
```

These commands will pull the desired images from `quay.io` and restart the container. Notice that there's no building involved with this command.

### A public release

We have a set of Aircloak installations that don't have their own individual releases, but run
our official production release. These systems are demo systems and customer facing Aircloak
hosted systems.

To upgrade these Aircloaks to the latest released production version
(the version specified in the [VERSION](./VERSION) file), run the
`./publish_public_systems.sh` command. It will update both the `air` and `cloak` container.

Just like the `./publish.sh` command, the `./publish_public_systems.sh` command uses the deployment
information from the [deploy_targets](./deploy_targets) folder. All [deploy_targets](./deploy_targets)
containing `PUBLIC_SYSTEM=true` will be deployed. No specific deploy target should be specified when
running the command.

This process of updating the system is separate from the process of creating the production
release itself. A [production release](https://github.com/Aircloak/aircloak/wiki/Releases) should
be made prior to the script being run.

## Producing production containers

The following two commands will build, tag, and upload new production `air` and `cloak` images to quay. It should be run from the root folder.

```
air/production.sh aircloak publish
cloak/production.sh aircloak publish
```

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
