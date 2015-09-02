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

All etcd instances are started automatically from `start_dependencies.sh`. If you need to apply some new settings, you can simply restart the container with `etcd/container.sh start`. This will cause previous instances of etcd to stop, and restart them again (in background).

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
4. Force reload configuration (by running `etcd/container.sh start`)

## Production settings

Non-sensitive production settings reside in `etcd_values_prod`. All sensitive settings (passwords, shared secrets) are stored on the server in the `etcd/local_settings/prod` file.
