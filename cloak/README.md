cloak-core
==========

| Branch      | Build status |
|-------------|--------------|
| develop     | [![Build Status](https://magnum.travis-ci.com/Aircloak/cloak-core.png?token=aFqD8qTNFV1Li4zdKtZw&branch=develop)](https://magnum.travis-ci.com/Aircloak/cloak-core) |

----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [API](#api)
    - [Database support](#database-support)
    - [Building, running and testing](https://github.com/Aircloak/org/wiki/tech::Running,-building,-and-testing-erlang-applications)
    - [Running locally](#running-locally)
    - [Building the sandbox](#building-the-sandbox)
    - [Testing](#testing)
    - [Good to know](#good-to-know)
- [Role in the greater picture](#role-in-the-greater-picture)
- [What it is made up of](#what-it-is-made-up-of)
    - [Components](#components)
    - [Database layer](#database-layer)
    - [The ring and data replication](#the-ring-and-data-replication)
    - [Tasks](#tasks)
        - [Batch tasks](#batch-tasks)
        - [Streaming tasks](#streaming-tasks)
    - [Plain inserts](#plain-inserts)
- [Making changes to cloak-core](#making-changes)
- [Auditor Guidelines](#auditor-guidelines)

----------------------

# What it does

cloak-core is the central component in a cloaked machine. It has numerous responsibilities, including:

- receiving and accepting data updates from clients
- distributing work across the cluster
- migrating data when a cluster undergoes changes
- anonymize results from tasks
- communicate results back to the air
- sending out anonymized application metrics

# Getting started
## API

cloak-core provides an HTTP API to the __air__ (__air__ as in the untrusted and unattested code base. In comparison to the cloak, which is both attested and trusted) which
partially accepts protocol buffers and partially JSON as inputs, and allows the outside to influence the behaviour of the cloak.

The HTTP endpoints exposed are declared in
[cloak_app](https://github.com/Aircloak/cloak-core/blob/develop/src/cloak_app.erl).

Of note are:

- `POST /task/run`: runs the task described with the input json body
- `POST /insert`: inserts the data for a single user
- `POST /bulk_insert`: inserts the data for multiple users
- `POST /migrate`: migrates analyst tables


## Database support

cloak-core stores all user data in Postgres. In order to test `cloak-core`, you should have a version 9.X of Postgres intalled (X >= 3).
Furthermore `cloak-core` assumes there is a `cloak` user and a `cloak` database.
For unit testing you additionally need databases and users called `cloaktest{1,2,3}`.

You can create (destructively) all the cloak and cloaktest users and databases using the `make regenerate_db` command.
It assumes that you have a user called `postgres` that is a superuser.
If you do not, please run the command: `CREATE USER postgres WITH SUPERUSER` in your postgres instance.

## Running locally

To start the system locally, use `make start`. It is possible to pass additional BEAM command line options by providing BEAM_ARGS parameter to the makefile. This allows you among other things to manually set application environment variables. For example, to configure the url where task results are reported, you can specify the value for the `air` key of the `cloak`:

```
$ make BEAM_ARGS="-cloak air '[{return_url, \"http://127.0.0.1:3000/results\"}]'" start
```

If you frequently run cloak-core locally and need values returned to a local version of
[web](https://github.com/aircloak/web), it might be useful to export the `BEAM_ARGS` in your `.bashrc` or
equivalent.

By default, `make start` starts the cloak in the development mode. If you want to change this, add `-cloak in_development false` to BEAM_ARGS.

### Running multiple nodes

In addition to `make start`, there are also `make start2` and `make start3`. Collectively these make it easy
to start a local multi-node cluster.
This is primarily useful when you want to test data replication and migrations locally.

All port numbers in the secondary nodes are `(N-1) * 100` higher than what they are in the primary `make start`-node.
For example: the http port in the node started with `make start` is __8098__, while it is __8198__ and
__8298__ for the `make start2` and `make start3` nodes respectively.


## Building the sandbox

To build the sandbox just run `make sandbox`.  This is automatically done for the `all` make target.
On some platforms (like Mac OS X) you might need to tweak the build process.  This is done by creating
`lua_sandbox/Makefile.local`.  An example for such a file is `lua_sandbox/Makefile.local-example-MacOSX`.

## Testing

Three different kinds of tests are integrated:

- Tests with `eunit` (`make test`, or `test/module module_name`),
- tests with `proper` (`make proper`), and
- extended tests with `proper` (`make proper-extended`).

Every test except the extended tests are run on Travis CI for every code change.
The extended proper tests have to be run manually, but require a quite potent system with enough memory.
At the moment you require around 4-6GB of RAM to run the full suite of extended proper tests.

The organisation of the tests is as follows:
- Unit tests are integrated in the corresponding module files.
- Proper tests reside in the `test/` directory.  There are three different kind of files in that directory.
  - Erlang source files that contain the proper tests and which are automatically executed by the rebar proper
    target from the rebar proper plugin.
  - The other two kinds are represented by all files of the form `test/full-*` and `test/extended-*`.  These
    files are _escripts_ (without the she-bang and the parameter line at the beginning of the file -- both are
    inserted automatically by the system) that get automatically executed by the `proper` and
    `proper-extended` _make_-targets.  The reason to use the _escripts_ is that we cannot shutdown `riak_core`
    without shutting down the full Erlang runtime and for the full tests and the extended tests we want to
    have a clean application start and cleared persistent storage for `riak_kv` and `query_vnode_cache`.
    Executing the tests and resetting the corresponding persistent information on disk is handled
    automatically by the `Makefile`.
  - If you want to print the standard output of the cloak node printed to the console use `make proper-verbose`

## Good to know

To speed up the development cycle, try the `make stage` target, which builds a release, and links in the
_ebin_ and _include_ directories of the development version. That way you can easily test the system without having to
continuously re-release.

Also try having the `reloader` app started, to automatically reload changed modules. It is part of webmachine,
so should already be present. Changes to supervisors are not caught, and might still require a restart of the
application.

In order to get reproducible builds, the dependencies are locked using rebar_lock_deps_plugin; run
 'make update-deps' to update the dependencies and generate a new rebar.config.lock file.

# Role in the greater picture

The cloak-core application is the centrepiece of our system. It delegates and distributes work and also
enforces the desired privacy properties for the data that leaves the cloaks.

# What it is made up of
## Components

For more in-depth documentation for the individual components, also be sure to check out the automatically
generated documentation under `./doc` that you get when running the `doc` make target.


## Database layer

All data about users is stored in the PostgreSQL database which runs on each cloak node. Each analyst has her own corresponding database schema that contains user data. Users are replicated and sharded across the cluster, meaning that each user is stored on some subset of all nodes in the cloak. In addition to user data, there are some application specific tables residing in the schema `aircloak`.

The database structure is maintained using the migration approach. While starting the cloak, we ensure that the essential database structure (`aircloak` schema) is up to date. This is done from the `sql_setup` module.

The migrations can themselves be executed in two separate situations. The most frequent scenario is when the analyst submits the migration through the web interface. This is then handled in the `migration_resource`. Less often, we need to synchronize the database when a new cloak node joins the cluster. This takes place in the `auto_table_migrator`. Both cases will basically delegate to `user_table_migration` which migrates all tables for all analysts.

The main entry point for all database operations is `cloak_db` which maintains a pool of open database connections. This module is used from various database related code to get the exclusive access to a database connection. The examples of client code include the aforementioned `user_table_migration` as well as `insert_worker` (user insertion) and `job_data_streamer` (user retrieval).

## The ring and data replication

The cloak partitions data by the id of the entity being stored into so called _partitions_. Each partition is
in turn stored on multiple distinct cloaks inside a cloak cluster to provide redundancy and data availability.

The mapping between id's and partitions, as well as the mapping of partitions and physical cloak nodes can
deterministically be calculated based on the meta data kept in a data structure called the _ring_.
The name comes from the fact that the partitions have numerical names that can be visualized as a ring, where
the highest count immediately proceeds partition 0.
At a high level the ring structure ensures that (given enough nodes), each partition is stored on __N__ distinct
nodes, allowing up to the loss of __N-1__ nodes, without ever loosing access to any user data.
For more information about the ring, please have a look at the implementation in the `ring`-module.

The `ring_manager` manages the ring that is in use by the cluster, and ensures that all cloak nodes agree on which
ring is currently in use. One instance of the `ring_manager` is running on each node in a cluster.
The `ring_manager` also notices when nodes are (temporarily) down, and marks this in the ring meta
data. The ring meta data makes it possible for the `ring_manager` to ensures that only
partitions that have a complete copy of the data are used for reading.

When a migration from one ring to a different ring takes place, the `ring_manager` coordinate the data transfer
and ring change through a two phase commit process that goes through the stages of `migration` and
`completion`. The `ring_manager`'s are conservative and abort a migration process if there is any indication
that it is not succeeding. This lowers the risk of data corruption.
The actual task of moving data from one cloak host to another during migration is performed by the
`partition_migrator`, which is started by the `ring_manager`.

When a new node joins an active cloak cluster, [manny-air](https://github.com/aircloak/manny-air) instructs
the cloaks in an existing cluster that the new node should become a part of the ring.
A `migration_daemon` periodically checks if there are new nodes that are waiting to join or be removed from a ring,
and if so instructs the `ring_manager` to start a migration to a new ring taking account of the pending changes.
Migrations are only started when no more nodes have been marked as about to join or
leave for a certain time. This avoid having to repeatedly do migrations for single node additions or removals,
as the migration process is quite costly in terms of time and resources.
While a node is waiting to become part of a ring, it can already be used as a destination for writes or task
executions. The `ring_manager` on the node knows about the ring in use by the rest of the cluster, and will
delegate write and read operations to the existing ring nodes.


## Tasks

The inputs to the cloak system are called _tasks_. A _task_ is a query that can be executed over multiple users. A query consists of two parts: _prefetch_ and _post processing_.

The prefetch part is essentially a set of database queries that will be used to fetch the initial data. This data is processed in the post processing phase. Here, lua code runs in a sandbox and has a chance to do something with the pre-fetched data, either by reporting some values, or enqueue some data to be stored in the database.

### Batch tasks

These tasks are executed on demand. The main entry point for tasks execution is the `task_coordinator` - a process that is in charge of running a single task. Each task runs in a separate `task_coordinator` process. Task coordinator merely distributes the task to each node in the cluster, and then collects all results and performs aggregation, anonymization, and reporting. For performance reasons each node pre-aggregates results before sending them back to the `task_coordinator` which only has to do a final aggregation.

On each node, task runs in the `task_local_runner` process where task execution is further split over each primary partition, using `task_partition_runner`. Each partition runner prefetches the data, groups it per each user, and then distributes to `job_runner` processes.

### Streaming tasks

This is a kind of task which runs continuously, computing the data on every insertion, and reporting it periodically.

A streaming task specification is similar to batch tasks, with following specifics:

- The prefetch section must consist of exactly one table, and must not specify filters.
- A report interval must be provided, specifying the period in which results are reported to the caller.
- A user expiry interval must be provided which defines for how long the reported data remains valid.

After a streaming task specification is uploaded to the cloak, its execution will be triggered by a user insertion. Whenever a user record is inserted into a primary partition, the corresponding sandbox code will run, and reported properties will be stored in the local SQL database. Previous properties reported for that user are deleted.

A cluster-wide, task-specific process will periodically collect all non-expired reported properties for the task. These properties are aggregated, anonymized, and reported to the task-related bucket cache. The bucket cache will keep the list of task properties in memory. New property will overwrite the current one with the same label only if the difference in noisy counts is greater than `2 * min_sigma`. After new buckets are reported to the bucket cache, the complete result is sent to the listener defined in the task specification.

The code dealing with streaming tasks resides in modules `streaming_task_monitor`, `streaming_task_reporter`, and `streaming_task_runner`.

## Plain inserts

In addition to tasks, it is possible to side-step the lua sandbox, and insert data directly into the database. In this mode, the classical task workflow is circumvented. Direct insertions are handled in `insert_resource` or `bulk_insert_resource` both of which delegate to the `user_inserter`. Here, the input JSON is parsed and validated before it is passed on to the `inserter` module.

Regardless of how the data is inserted, from a task or as a result of an external request, various validations are performed in the `insert_validator` module.


# Making changes to cloak-core

It is very important that you keep the documentation in the _docs_ sub-folder in sync with the code if you
change the implementation.
Please update the documentation of the anonymization procedure, and the dataflow.
If you use another method for the range generation, please update the corresponding document or remove it!

# Auditor Guidelines

`cloak-core` uses a web server framework for communicating with `nginx`.  Specifically, `cloak-core` uses the mochiweb and webmachine frameworks for this purpose.  Both mochiweb and webmachine can be found on github.com (at https://github.com/mochi/mochiweb and https://github.com/basho/webmachine respectively).  Note that some other Erlang repositories also use this framework.

https://github.com/basho/webmachine/wiki/Resource-Functions documents the various routines that are called, for instance in response to various HTTP requests (`post_is_create()`, `create_path()`, etc.) or other events.

Likewise, https://github.com/basho/webmachine/wiki/Request-Data-API documents how the body of web requests (i.e. POST) are retrieved.

`cloak-core` (and other Erlang repositories) uses rebar as its Erlang build tool (see https://github.com/basho/rebar).  The file `rebar.config` lists the complete Erlang code that `cloak-core` is dependent on.  The plugin `rebar_lock_deps_plugin` is used to lock the versions given in `rebar.config` to specific git commits, listed in `rebar.config.lock`.  The auditor may use this to determine the exact Erlang source code used in the cloak image.

The file `cloak_app.erl` defines which modules are executed when various requests are received (from `nginx`).  This file also contains the init routine that starts the different components of `cloak-core`.

`cloak-core` generates log messages.  The auditor may ensure that no log messages leak user data.  `cloak-core` uses the lager framework for logging (see https://github.com/basho/lager).  `cloak-core` log messages are routed to syslog using the basho lager_syslog package (see https://github.com/basho/lager_syslog).  Its use is specified in `rel/files/app.config`.  `app.config` also disables crash and error logging to avoid leakage of user data through these channels.  Instead, errors are logged with a custom error handler, contained in `cloak_error_logger_handler.erl`.  A routine in this file also ensures that all other error loggers are disabled.  Macros for lager logging messages are defined in `cloak_logging.hrl`.
Note that `cloak-core` logging can operate in one of two modes: development and production.  The previous paragraph describes production mode.  In development mode, all events are forwarded to the standard lager-supplied error event logger.  The routine `in_development` in `cloak_conf.erl` determines which mode to use based on the existence of the file `/aircloak/common/config/develop`.  The auditor may check that this file does not exist.  Note also that other Erlang-based components, in the repositories `erlattest`, `ipsecman`, and `manny-core`, use the same logging framework, but they operate only in development mode, since they do not handle user data.

`cloak-core` reports performance metrics. The auditor may ensure that these metrics do not leak user data.  To mitigate this possibility, metrics are themselves anonymized.  The routines `cloak_metrics:count()` and `cloak_metrics:histogram()` result in metrics being externally reported via TCP port 2004.  The cloak_metrics software, which is in a separate repository at `Aircloak/cloak-metrics`, runs within the `cloak-core` process.  The transmit call itself is at `Aircloak/cloak-metrics/cloak_metrics_tcp_transport.erl`.

Processes within `cloak-core` (both in the same cloak and in other cloaks in the same cluster) have several ways of passing messages to each other.  These include:

* Erlang inter-process message communications using the `!` notation to send, and `receive` to receive.

* gen_server:  Transmits are `gen_server:cast`, `gen_server:call`, etc., with corresponding callbacks `handle_cast`, `handle_call`, and `handle_info` to receive.

* RPC: Transmits are `rpc:cast`, `rpc:abcast`, `rpc:multicall`, etc.  These result in the named Erlang functions being called, with data transmitted as function arguments.

When communications with processes in other cloaks takes place, the messages are transmitted via TCP connections established over port 34423 (over IPSec).  (The Erlang-internal port mapping service epmd uses ports tcp:4369 and udp:4369 over IPSec.)

A `cloak-core` Erlang process also communicates with the sandbox, which is written in C, using a pipe.  The pipe is created with `open_port`.

All data entering `cloak-core` from external sources (not including `cloak-core` instances in other cloaks) comes in via `nginx`.   URLs with the endpoints `/bulk_insert` and `/insert` are used to transmit user data to cloaks.  URLs with the endpoint `/lookup` are used to transmit auxiliary (non-user) data to cloaks.  URLs with the endpoint `/task` submit tasks, including queries, to cloaks.  Tasks can read user data (limited by the sandbox to one user at a time) and auxiliary data (not limited by the sandbox).  All other URLs are used for various management and monitoring functions.

If user data is accidentally transmitted to a cloak using the `/lookup` URL, then this data may be exposed by a task.  This accidental exposure is made less likely due to the fact that the credentials needed for `/lookup` are different from those needed by `/insert` and `/bulk_insert`.

The auditor may validate that user data accidentally transmitted to a cloak using any of the management/monitoring URLs cannot result in that user data exiting the cloak.

When the URL has the endpoint `/admin/ring`, the admin_resource module (`admin_resource.erl`) is used.  This is for managing the addition and removal of cloaks from the cluster "ring".  Only POST and DELETE HTTP methods are allowed (see `allowed_methods`).  POST is used to join ring, DELETE to leave ring or remove other node from ring.  The `ring_manager` software manages these functions, and communicates with rpc using port 34423, which is configured in `rel/files/app.config`.  No other communications interfaces are used by this module.

When the URL has the endpoint `/migrate`, the migration_resource module (`migration_resource.erl`) is used (POST method).

When the URL has the endpoint `/status/type`, where type is `ring_health`, `migration`, or `peers`, the status_resource module (`status_resource.erl`) is used.    This is GET only, and is called from `manny-core`, not `nginx`.

When the URL has the endpoint `/lookup/action`, where action can be `upload` or `remove`, the lookup_resource module (`lookup_resource.erl`) is used (POST method).  `resource_common:get_body(Req)` is where the POST body is grabbed.  This data goes into the migration functions, which ultimately result in a load into Postgres from `analyst_tables.erl`, using `sql_conn:extended_query`.

When the URL has the endpoint `/bulk_insert`, the bulk_insert_resource module is used.  Likewise, when the URL has the endpoint `/insert`, the insert_resource module is used (both POST only).  Since these POSTs contain user data, the auditor may inspect this code carefully to ensure that the user data is only transmitted to Postgres.  User data is inserted into Postgres using the `sql_conn:simple_query`, `sql_conn:batch_query`, and `sql_conn:extended_query` calls.

When the URL has the endpoint `/task/run`, the task_resource module is used (method POST).  In `task_coordinator.erl` the task is transmitted to other cloaks (`gen_map_reduce:start_supervised`).  `task_partition_runner.erl` is where the actual task is run (data from sql, partition per user, run lua task).  `task_coordinator.erl` also collects the results, and calls anonymizer (`anonymizer.erl`).  `job_runner.erl` is where the communications with sandbox takes place (`open_port` opens port, `port_command` sends to port, `handle_info`).  The auditor may carefully inspect this code to ensure that user data passes through the anonymization function, and is not otherwise transmitted.
