# AircloakCI User's Guide

This document describes how to use AircloakCI to test different parts of the Aircloak project. If you want to know more about developing and operating the CI component, take a look [this document](./README.md).

## Overview

The AircloakCI is a component which tests the Aircloak system. AircloakCI runs on our internal server, monitors the Github repository, and on every relevant change runs the corresponding builds.

In AircloakCI terminology, a _build_ is a workflow executed on the snapshot of the repository. AircloakCI runs build for every change on the master branch, release branches, and pull requests. In addition, nightly builds are executed once a day on the master and release branches.

Every build runs various jobs on Aircloak components. A _component_ is a subproject inside the Aircloak repository, such as air or cloak. Currently, only direct subfolders of the root folder are considered as components. A _job_ is a series of commands bundled under the single name. The most frequent jobs are `compile` and `test`. There is also a special job called `compliance` (compliance tests of the cloak component). Finally, each component can define multiple nightly jobs.

A _command_ is an OS command executed in the docker container of the component. When executing the build, AircloakCI will build the docker image for each component. Then, for each job, the corresponding docker container is started. Finally, AircloakCI executes the job commands in the container.

## Setting up component tests

To include a component in the test, you need to create the `ci` folder under the component folder. In that folder you need to do the following:

- create the AircloakCI interface script
- describe the docker image
- describe the jobs

### Interface script

The interface script is invoked by AircloakCI to build the docker image, start the docker container, and execute commands in the container. The script must reside in the `ci/container.sh` file which must be executable. Here's a simple example of the interface script for the bom component:

```bash
#!/bin/bash

set -eo pipefail

ROOT_DIR=$(cd $(dirname ${BASH_SOURCE[0]})/../.. && pwd)
cd $ROOT_DIR

# load the CI helper library, passing the component name as the argument
. docker/ci_helper.sh bom

# describes which files and folders will be mounted to the container
mount_to_aircloak VERSION common/elixir
mount_to_component config lib priv test mix.exs mix.lock Makefile .gitignore check_warnings.sh .formatter.exs

# describes which cached files and folders will be mounted to the container
mount_cached_component deps _build

# Perform the proper action, depending on the first argument.
case "$1" in
  prepare_for_test)
    :
    ;;

  *)
    # delegates handling to the function from `docker/ci_helper.sh`
    default_handle "$@"
    ;;
esac
```

Essentially, the script needs to:

1. load the CI helper library, passing the component name as the argument
2. mount required files and folders to the container
3. mount cached files and folders to the container
4. perform the proper action, depending on the first argument

In steps 2 and 3 we're mounting some files to the container. The difference is that in step 2 we're mounting source files, while in step 3 we're mounting cached files (which reside in the `aircloak/tmp` folder). Typically, in step 2 you should mount the required input source files, while in step 3 you should mount things which should be cached (e.g. deps and _build folders).

In step 4 you need to analyze the first argument and perform the corresponding action. In most cases, this can be delegated to the generic `default_handle` function (which is defined in `docker/ci_helper.sh`).

The only mandatory action you need to handle in the script is `prepare_for_job` (e.g. `prepare_for_test`, `prepare_for_nightly`) which is invoked after the container is started, but before the job commands are executed. The role of this action is to execute any additional commands which will setup the environment (e.g. start additional supporting containers). Even if you don't need to do anything in the preparation phase, this action must be handled, e.g. by executing a noop (`:`).

### Describing the docker image

The docker image is described in `ci/dockerfile`. You can also optionally include `ci/.dockerignore`. The image is built when the CI server invokes the container script with the `build_image` argument. The `default_handle` function will then build the base images (Phoenix and Rust), and then the component image.

### Specifying jobs
The jobs can be specified in `ci/jobs.exs` and `ci/nightly.exs`. The former is used to list jobs executed during regular build (on every PR and branch change). The latter is used to specify jobs which are executed once a day between midnight and 4 AM. Both files are optional. The absence of each file means that there are no jobs defined.

#### Regular jobs

The regular jobs are described in `ci/jobs.exs`. Here's an example:

```elixir
%{
  compile:
    {:sequence,
     [
       "make deps",
       "mix compile",
       {:parallel,
        [
          "MIX_ENV=test mix compile",
          "MIX_HOME=_build mix dialyze --no-analyse"
        ]}
     ]},

  test:
    {:sequence,
     [
       "make deps",
       "MIX_ENV=test ./check_warnings.sh",
       {:parallel,
        [
          "MIX_ENV=test mix lint",
          "MIX_ENV=test mix test"
        ]}
     ]}
}
```

The `job.exs` script must return a map where keys are job names, and values are commands executed as the part of the job.

The job name can be one of the following: `:compile`, `:test`, `:compliance`. AircloakCI will execute these jobs in particular moments, depending on the build source. For example, in branch builds, only the `:compile` job is executed. In PR builds, `:compile` is first executed, and on success, `:test` is executed. The `:compliance` build is executed only after standard tests have passed, and the PR is approved.

The value (type `job_commands`) can be one of the following:

- `String.t()` - single command
- `[job_commands]` - multiple commands executed in sequence
- `{:sequence, [job_commands]}` - same as `[job_commands]`
- `{:parallel, [job_commands]}` - multiple commands executed in parallel

All of the commands will be executed in the container of the component.

#### Compile vs test jobs

The distinction between compile and test jobs deserves a special mention. This distinction is encoded into AircloakCI, and it serves the caching purposes. On branch builds, AircloakCI invokes only compile jobs, to keep the caches (contents of the `aircloak/tmp` folder) up to date. The caches from the source branch are then copied to the PR build during its initialization.

Therefore, it's worth including all commands which create build artefacts in the `:compile` job. This includes deps fetching and compiling, but also some other actions, such as PLT building (`MIX_HOME=_build mix dialyze --no-analyse` in the example above).

Another important thing of note is that when `:test` job is executed, it's not guaranteed that the state on the disk is up to date. Such situations are not very likely, but can still happen when another PR has been merged after the `:compile` job has been executed, but before the `:test` job has been started. In such cases, some `:compile` commands must be executed again.

Therefore, in the example above we include `make deps` in the `:test` job too. No other command from the `:compile` job is included in `:test`, since all other compile tasks (compilation, PLT build) will be performed implicitly if needed (due to how `mix` works).

### Nightly jobs

These jobs are described in `ci/nightly.exs`. Here's an example:

```elixir
%{
  system_test: {:sequence, ["make", "make test"]}
}
```

The structure is the same as in `jobs.exs`. Unlike with regular jobs, the names of these jobs can be arbitrary, and are used only for logging purposes.

Every nightly job will be executed once a day between midnight and 4AM. The jobs are executed one at a time on master and release branches.
