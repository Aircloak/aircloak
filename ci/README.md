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

Note that you don't need to use this service to run the CI suite locally. You can invoke shell scripts directly, or via make tasks. For example, to run and debug compliance tests, you can invoke `make ci.compliance` and `make ci.compliance.debug` tasks from the `cloak` folder. See `cloak/README.md` for more details.


## Production

The service is running as a systemd service called `aircloak_ci` on acatlas4.mpi-sws.org. The service definition file is located [here](./production/aircloak_ci.service). You can use standard systemd tools, such as `journalctl` and `systemctl` to manage the service. There is also the local `production.sh` script, which simplifies some common tasks:

- reading the service log: `./production.sh service_log`. This command is a wrapper around `ssh acatlas4 "journalctl -u aircloak_ci --no-pager ..."`, so you can pass standard `journalctl` arguments. For example, to tail the production log, you can invoke `./production.sh service_log -f`
- deploying the new version: `./production.sh deploy`
- rolling back to the previously deployed version: `./production.sh rollback`

Following commands allow you to work with a particular build job (e.g. compliance test for PR 1234):

- getting the snapshot of the build log: `./production.sh target_type target_id job_name`
- force starting a build: `./production.sh force_build target_type target_id job_name`

Where:

- `target_type` is either `branch` or `pr`
- `target_id` is branch name or PR number
- `job_name` is one of `cloak_compile`, `cloak_test`, `compliance`

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
