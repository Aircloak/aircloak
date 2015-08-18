Air etcd
============

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [Running](#running)
    - [Overriding settings](#overriding-settings)
    - [Production settings](#production-settings)

----------------------

# What it does

This component powers the registry with Air settings, such as various urls, shared secrets, and similar system-wide settings. Other components can access etcd registry to obtain those settings.

# Getting started

## Running

In most cases, you'll want to power your local components. In this case, simply call `etcd/container.sh start` (make sure Docker is started). This will cause previous instances of etcd to stop, and restart them again (in background). The environment will be configured for local components and locally based cloak. Moreover, another `etcd` instance will be started, which listens on port 4004 and is used by tests.

If you need to run your components in docker containers, then start etcd as just described, and then call `etcd/config_docker.sh` which will change some values, so docker containers can talk to each other. If you want to revert back to local settings, just run `etcd/config_local.sh`.

## Overriding settings

Settings are defined in following files under `etcd` folder:

- `etcd_values_dev`
- `etcd_values_test`
- `etcd_values_docker` (auto-generated from `etcd_values_dev`)
- `etcd_values_prod`

If you want to override some of those settings, you can do following:

1. Create `etcd/local_settings` folder.
2. Depending on which environment are you configuring, in that folder create `dev`, `test`, `docker`, or `prod` file
3. Copy whichever settings you want to override from `etcd_values_*` to corresponding file in `etcd/local_settings` and set new values.
4. Force reload configuration (by running `etcd/config_*.sh`)

## Production settings

Non-sensitive production settings reside in `etcd_values_prod`. All sensitive settings (passwords, shared secrets) are stored on the server in the `etcd/local_settings/prod` file.
