# AircloakCI

- [What it does](#what-it-does)
- [Getting started](#getting-started)
- [Production](#production)

## What it does

This project implements our custom CI service which polls Github for changes on the [aircloak/aircloak](https://github.com/aircloak/aircloak) project, runs tests on pending pull requests, and reports results via PR comments.


## Getting started

### Prerequisites

In order to run the system you need Erlang and Elixir. If you followed the [prerequisites](../README.md#prerequisites) section in the main README, you should already have the right version of the three first installed and managed through `asdf`.

You also need docker 17.09 or greater. On Linux systems, your user must be a member of the `docker` group.

You need to configure the Github access via `git`, so that `git` commands (such ask `clone` and `fetch`) work properly.

Finally, you need to create the personal access token for your Github account. You can do that on [this page](https://github.com/settings/tokens). Make sure to check all the boxes under the `repo` section. Once the token is created, you need to create the folder `~/.aircloak_ci`, and in that folder create the file `config.json` which has the following shape:

```json
{
  "github_access_token": your_access_token
}

```

### Running

You can force start a build with `mix aircloak_ci.force_pr_build pr_number`. The pull request needs to be mergeable (no conflicts with the target branch).

You can start the local service by invoking `make start`. This will behave exactly as the real service, except it will use your credentials, and it will not post comments and status changes to Github.

You can make some changes to the local behavior by creating the `config/dev.local.exs` file. See `config/dev.local.exs.example` for details.

All checkouts and logs reside in `~/.aircloak_ci/data`. In case something is terribly broken, you can remove that folder (but not the `~/.aircloak_ci` folder), and try again.

Note that you don't need to use this service to run the CI suite locally. You can invoke shell scripts directly, or via make tasks. For example, to run and debug compliance tests, you can invoke `make ci.compliance` and `make ci.compliance` tasks from the `cloak` folder. See `cloak/README.md` for more details.


## Production

The service is running as a systemd service called `aircloak_ci` on acatlas4.mpi-sws.org. The service definition file is located [here](./production/aircloak_ci.service). You can use standard systemd tools, such as `journalctl` and `systemctl` to manage the service. There is also the local `production.sh` script, which simplifies some common tasks:

- reading the service log: `./production.sh service_log`. This command is a wrapper around `ssh acatlas4 "journalctl -u aircloak_ci --no-pager ..."`, so you can pass standard `journalctl` arguments. For example, to tail the production log, you can invoke `./production.sh service_log -f`
- deploying the new version: `./production.sh deploy`
- rolling back to the previously deployed version: `./production.sh rollback`

Following commands allow you to work with a particular build job (e.g. compliance test for PR 1234):

- getting the snapshot of the build log: `./production.sh target_type target_id job_name`
- force starting a build: `./production.sh force_build target_type target_id job_name`
- starting interactive remote console on the CI server: `./production.sh remote_console target_type target_id component`

Where:

- `target_type` is either `branch` or `pr`
- `target_id` is branch name or PR number
- `job_name` is one of `component_compile`, `component_test`, `compliance`, or `all`
- `component` is one of `air`, `cloak`, `central`, `bom`, `common`, `ci`, `integration_test`

For example, to restart the cloak_test build for branch `my_branch`, you can invoke:

```
./production.sh branch my_branch cloak_test
```

The service is running under the `ci` user. Releases reside in the `/home/ci/aircloak_ci` folder. All temporary files (local caches and logs) reside in the `/home/ci/.aircloak_ci/data/` folder.

In case you want to get a remote iex shell session, you can do it with the following command sequence:

```
ssh acatlas4.mpi-sws.org
su ci
/home/ci/aircloak_ci/production/current/bin/aircloak_ci remote_console
```

In case you need to stop/start/restart the service, you can do it with the following command sequence:

```
ssh acatlas4.mpi-sws.org
systemctl stop/start/restart aircloak_ci.service
```

### Starting a component CI container

It is possible to start a component CI container, both locally and in production, and then manually run various commands, such as compile, test, ...

To start a local CI container for a component, you can invoke `ci/start_component.sh component_name`. For example, to start a container for the `bom` component, you can invoke `ci/start_component.sh bom`.

To start a CI container on a production, you need to invoke the following commands:

```
$ ssh acatlas4.mpi-sws.org
$ su ci
$ cd /home/ci/.aircloak_ci/data/cache/builds/pr-XYZ/src
$ MPI=true ci/start_component.sh component_name
```

Notes:

- `start_component.sh` is not available for PRs targeting the release 18.1 branch.
- `start_component.sh` also starts the supporting containers, such as database server.
- No initialization is performed. You need to manually fetch dependencies, and initialize database.
- Refer to `component_folder/ci/jobs.exs` for the exact shape of test commands.

### Internal folder structure

The component keeps all the data in the `~/.aircloak_ci/data` folder. In production, this is the `/home/ci/.aircloak_ci/data` folder.

The `~/.aircloak_ci/data/logs` folder contains the build logs. The folder name for a pull requests is in the shape of `pr-XYZ`. For example, build logs for PR 1234 are in the folder `~/.aircloak_ci/data/logs/pr-1234`. In the log folder, you'll find a bunch of logs, which are plain text files. You can analyze these logs, in case the default report doesn't provide enough information.

The build sources are stored in the `~/.aircloak_ci/data/cache`. In this folder, you'll find two subfolders: `branches` (branches sources) and `builds` (PR sources). The actual source is then stored in the `src` folder of the target. For example, source for the PR 1234 sits in the folder `~/.aircloak_ci/data/cache/builds/pr-1234/src/`. In this folder, you can start CI containers, as explained in the previous section.

### Manually fixing a build

Occasionally a build might end up in a weird state, such as a dependency which cannot be compiled. While this shouldn't happen, there's always a possibility of a weird bug in the CI server which causes a corruption of the local cache. If you want to proceed forward without going down the rabbit hole of tracking the CI bug, you can try to manually fix the build.

You can manually investigate and fix the build with following steps:

- Ssh to the CI server and invoke `su ci` to impersonate the CI user.
- Go to the folder of your build, as explained in the previous section (e.g. `~/.aircloak_ci/data/cache/builds/pr-XYZ/src/`).
- Checkout the git status first (what is the head, and whether there are some files missing). Be aware that the CI server checks out a detached merge commit. If needed, you can `git reset HEAD --hard` or invoke some other commands to bring the working copy to the clean state.
- Invoke `MPI=true ci/start_component.sh component_name` to start the docker container for the desired component.
- Invoke individual commands which failed. You can freely remove any build artifact (e.g. a particular dependency, or the whole build folder), and invoke standard commands, such as `mix deps.get`. Whatever you do inside the docker container will be reflected on the cache for your build, since all the files are mounted.
- Once you manage to manually build from the docker container, exit the container and force start the failed CI build.

### Setting up the server

The initial configuration of the production server is manual. This section describes the needed steps to set up a new CI server.

#### Prerequisites

- Debian
- Docker (at least 17.12)
- git
- [asdf](https://github.com/asdf-vm/asdf) and the required plug-ins (Erlang, Elixir, node)

#### Setup

1. Create a non-root user with the login `ci` and add it to the `docker` group.
2. For the `ci` user, upload the Github private key, and setup ssh access in `/home/ci/.ssh/config`:

    ```
    Host github.com
    User git
    PreferredAuthentications publickey
    IdentityFile ~/.ssh/github/id_rsa
    ```

3. Make sure that proxies are properly configured at `/home/ci/.docker/config.json`
4. Create the `/home/ci/.aircloak_ci/config.json` with the following content:

    ```
    {
      "github_access_token": github_api_access_token
    }
    ```

5. As the `ci` user, create the `/home/ci/aircloak_ci` folder, and in that folder invoke `git clone git@github.com:aircloak/aircloak build`
6. If the server name has changed, you need to update the `ci/production.sh` on the development machine.

At this point, you can deploy the system from your development machine.
