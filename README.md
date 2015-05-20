air
==========

This repository contains all components needed to run the air system:

- `etcd` - Dockerized KV registry where system configuration is stored. All other components retrieve their settings from here (e.g. database settings, or addresses of other services)
- `db` - Dockerized database server (used only for development and testing)
- `backend` - Erlang system which implements various processes in the air system, such as background and periodic jobs.
- `frontend` - Web user interface

This document explains how to set up and run these components locally. A more detailed description of each component is available in the `README.md` in the corresponding folder.


# Running the system locally

There are two ways of running the system:

1. Run most of the components on a localhost.
2. Run each component inside a docker container.

Regardless of the approach you take, etcd and database server are always running as docker containers.

The first approach is the one you should usually use for standard development. Here you run almost everything locally (save etcd and db), which makes it simpler to develop, experiment, and test.

The second approach allows you to build docker containers for each component, allowing you to test the production system on your local machine.

## Prerequisites

In order to run the system you need the following components:

- Docker 1.6 (+ boot2docker if on OS X)
- Ruby 2.0
- Erlang 17.5
- Any other package needed to build and run specific components (e.g. liblua, libprotobuf, ...)

__Linux developers__: Scripts in this project use docker in the context of the logged in user (without root
privileges). To enable this, you need to add yourself to the `docker` group. See
[here](http://askubuntu.com/a/477554) for explanation.

### OS X and boot2docker

On OS X machine, docker service is running inside boot2docker VM, so some additional setting up is needed.

#### Starting and intializing boot2docker

After your localhost is booted, you need to start boot2docker with `boot2docker start`

Then, in each shell session you need to run `eval $(boot2docker shellinit 2>/dev/null)`

You can consider putting these statements inside `~/.bash_profile`

#### Port forwarding

You need to forward following ports from your localhost to boot2docker VM:

- 4002 (etcd)
- 4003 (etcd for unit tests)
- 5433 (database server)
- 8080 (frontend)

#### Mounting non-home folders

If your repository is located outside of your home directory, you need to mount the top most
parent folder to boot2docker VM under the same location. For example, if this repo is locally situated under
`/projects/aircloak/web`, you need to map your local `/projects` folder to `/projects` on boot2docker VM.

One way to do this is to share the folder via VirtualBox and auto mount it on boot2docker boot:

1. Add `projects` as the shared folder in VirtualBox under some name (e.g. PROJECTS)
2. Make sure boot2docker VM is started. Run `boot2docker ssh` and create the file `/var/lib/boot2docker/bootlocal.sh` with the following contents:
```
sudo mkdir -p /projects
sudo mount -t vboxsf -o uid=1000,gid=50 PROJECTS /projects
```
3. `chmod +x /var/lib/boot2docker/bootlocal.sh`
4. Exit the machine and run `boot2docker restart`
5. Verify that `/projects` is available on your boot2docker machine.

Alternatively, you can setup an NFS mount. You can use a helper script in the root of this repository:

1. Make sure you have `nfs.server.mount.require_resv_port = 0` in your local `/etc/nfs.conf`
2. Run `./osx_mount_nfs /projects`
3. Verify that `/projects` is available on your boot2docker machine.

__Note__: if using NFS approach, you need to mount folders after each restart of boot2docker VM.


## Starting etcd and database

These two containers are always needed for the rest of the system. To start them, simply run:

```
$ etcd/start-container.sh
$ db/start-container.sh
```

### One-time database migration

In the new setup, the database runs inside the docker container, and listens on the port 5433. If you have air database on the localhost, you probably want to migrate this data to the new database:

1. Make sure etcd and database containers are running
2. OS X users: make sure port 5433 is forwarded from your localhost to boot2docker
3. Make sure that both docker and your local databases are migrated. Migrations are done from the `frontend` folder by running `bundle exec rake db:migrate`. You can migrate your local database by temporary hardcoding settings in `database.yml` and running the migration. Then revert back `database.yml` and run migration once more to migrate the docker database. Finally, make sure to run `RAILS_ENV=test bundle exec rake db:migrate` to migrate the test database.
4. On your localhost (outside of any VM) run
   `pg_dump --data-only --no-owner --no-acl -h localhost -U postgres aircloakdatabase | psql -h 127.0.0.1 -p 5433 -U postgres aircloakdatabase`
   This assumes that your old database is called `aircloakdatabase`. If that's not the case, change the name in the left part of the command (before the `|` character), leaving the right part untouched. You'll see various errors, but data should be migrated. You can check that manually by inspecting the content of some table on `localhost:5433` database server.

__OS X devs__: Keep in mind that your database data is stored inside boot2docker VM in
`/mnt/sda1/docker_volumes/air_db` folder. __If you delete boot2docker VM, this data will be lost.__

## Running the system on the localhost

Start `etcd` and `db` containers and configure the system to run locally:

```
$ etcd/start-container.sh
$ db/start-container.sh
$ etcd/config_local.sh
```

Make sure that all dependencies have been fetched, and that needed components (e.g. backend) have been built.

Now you can start frontend and backend in the usual way:

```
web/frontend $ bundle exec rails s
web/backend $ make start
```

If all is well, you should be able to access the web via `localhost:3000`. If all data is migrated, you should see all clusters/cloaks (make sure to impersonate the analyst), and run tasks in the sandbox.

## Running the system on docker containers

Start `etcd` and `db` containers and configure the system to run on docker containers:

```
$ etcd/start-container.sh
$ db/start-container.sh
$ etcd/config_docker.sh
```

Build images and start containers:

```
$ backend/build-image.sh
$ backend/start-container.sh

$ frontend/build-image.sh
$ frontend/start-container.sh
```

If everything is fine, you chould be able to access the web via `localhost:8080`.
