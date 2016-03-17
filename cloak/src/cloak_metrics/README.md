cloak-metrics
==========

| Branch      | Build status |
|-------------|--------------|
| develop     | [![Build Status](https://magnum.travis-ci.com/Aircloak/cloak-metrics.png?token=aFqD8qTNFV1Li4zdKtZw&branch=develop)](https://magnum.travis-ci.com/Aircloak/cloak-metrics) |


----------------------

- [What it does](#what-it-does)
- [Getting started](#getting-started)
    - [API](#api)
    - [Dependencies](#dependencies)
    - [Building, running and testing](https://github.com/Aircloak/org/wiki/tech::Running,-building,-and-testing-erlang-applications)
    - [Local testing](#local-testing)
- [What it is made up of](#what-it-is-made-up-of)
    - [Components](#components)
    - [Supervision tree](#supervision-tree)

----------------------

# What it does

cloak-metrics is an OTP application that can be used from other projects to gather various statistics about
system's behavior and report these statistics to custom reporters (e.g. Graphite compliant reporter).

You can use its functions from your own code to collect arbitrary metrics. Every 10 seconds, the collected
metrics are aggregated, anonymized, and sent to reporters. Out of the box, the Graphite pickle compliant
reporter is provided.

In practice, this OTP application runs inside the `cloak-core` system. It's also worth noting that this repository contains provisioning script for the Graphite server. In our production environment, this server doesn't belong to the `cloak`, and instead runs in the `air` part of the system.

There are two distinct metric types supported: _counters_ and _histograms_. All provided values
must be integers.

A _counter_ is simply an incrementing integer. At the end of the collection period, the corresponding rate per
seconds is emitted. E.g. if the collection interval is 10 seconds, and the total count of some counter is
50, then the emitted value is 5 (meaning 5 times per second).

For _histograms_, all datapoints are collected individually, but the output are aggregated metrics, such as
average and median. At the end of the collection period, datapoints are bucketized and anonymized
(corresponding functions must be provided via input parameter). Then, various statistics are emitted, such as
average, median, and percentile upper bounds. Note: the count per histogram is not reported. You should
explicitly use counter to gather count statistics.

# Getting started

## API

All relevant functions are contained in the `cloak_metrics` module. You can use it to start the collection
server, and collect metrics values.

For detailed reference you can build documentation using `make doc` from the root folder, and opening
`doc/index.html`.

### Starting the collector

Assuming that `cloak_metrics` OTP application is started, the collector can be started with
`cloak_metrics:start_server/1` function:

```erlang
cloak_metrics:start_server([
  % required - if you don't need bucketizer, pass {bucketizer, cloak_metrics:identity_bucketizer()}
  {bucketizer, fun make_buckets/1},
  % required - if you don't want to apply noise, pass {noise_fun, cloak_metrics:no_noise_fun()}
  {noise_fun, fun add_noise/1},
  % optional - default value is 10 seconds
  {flush_interval, Msec},
  % optional - names that are prepended to each emitted metric name
  {path_prefix, [my_prefix]},
  % required - list of reporters that dispatch statistics to reporting components (see docs for details)
  {reporters, [
    cloak_metrics:graphite_pickle_reporter("localhost", 2004)
  ]}
])
```

This will start the locally registered process that handles all subsequent metric requests.

The server runs under `cloak_metrics` supervision tree, so you don't need to add it to your own supervisor.
Only one instance of the server can exist. Multiple calls of `cloak_metrics:start_server/1` will result in
error.

### Pushing metrics

Once the server is running, you can simply push metrics via `cloak_metrics` functions:

```erlang
% increments a counter (default Increment is 1)
cloak_metrics:count("cloak.query"),
cloak_metrics:count("cloak.query", Increment),

% adds datapoint to a histogram
cloak_metrics:histogram("cloak.query", SomeValue),

% collects lambda duration and adds it to a histogram, returns lambda's result
cloak_metrics:measure("cloak.query", fun() -> ... end),
```

In metric names only alphanumeric, underscore, dash (-), and period (.) characters are allowed. Other
characters will be removed from the output. For metric names you can use atoms, strings, or binaries.

In Graphite, each part between period characters represents a parent node. The host name will be automatically
prepended to the resulting name. Finally, you can set global prefixes by providing
`{path_prefix, [Prefix1, Prefix2, ...]}` option to `cloak_metrics:start_server/1`. Each prefix can be an
atom, string, or a binary. The full prefix for each metric is then `host_name.prefix_1.prefix_2.prefix_3`.

Since the period character is used as a path delimiter, periods in host name
will be replaced with the underscore character. The metric name will be added as the last component of the
full name. For the example above, following metrics will be generated:

- host_name.my_prefix.cpu.usage.value
- host_name.my_prefix.cloak.query.rate
- host_name.my_prefix.cloak.query.average
- host_name.my_prefix.cloak.query.median
- host_name.my_prefix.cloak.query.upper_75
- host_name.my_prefix.cloak.query.upper_90
- host_name.my_prefix.cloak.query.upper_99

As a consequence, if you use the same name for two type of metrics, they will reside under the same node in
Graphite. This is by design, since it allows you to group different data of some observed metric. For example,
if you have the following code:

```erlang
run_query(...) ->
  cloak_metrics:count("cloak.query"),
  cloak_metrics:measure("cloak.query", fun() -> ... end).
```

Then in Graphite you will have following data:

- host_name.my_prefix.cloak.query.rate
- host_name.my_prefix.cloak.query.average
- host_name.my_prefix.cloak.query.median
- host_name.my_prefix.cloak.query.upper_75
- host_name.my_prefix.cloak.query.upper_90
- host_name.my_prefix.cloak.query.upper_99

**Note**: Current implementation will in addition generate `anonymization_error.*` for all output metrics. Here the relative percentage error introduced by anonymization can be observed.
This is a temporary solution, so we can observe how anonymization affects produced statistics.

### Noise and buckets

The noise function is applied to the counter before its value is emitted. Its purpose is to distort the input
value.

The bucketizer function is applied to collected histogram datapoints. It must somehow group data into similar
non-overlapping buckets in the form of `{From, To, Count, NoisedCount}`. A bucket holds value in the
right-open interval `[From, To)`.

## Server setup

If you want to setup the server, you can use scripts from `provisioning/setup_server` folder. This folder
must be transfered to the target server. Then the `setup` script should be run under admin privileges. The
script assumes a clean state (nothing is installed), creates the user that will run required components,
installs all necessary packages and sets up the configuration and init scripts.

After the setup, all components are automatically started. Corresponding init scripts are configured, and
the system will automatically start during the boot of the host machine.

## Local testing

If you want to test with a local Graphite, there is a [Vagrant](http://www.vagrantup.com/) Debian box
provided. Assuming you have Vagrant installed, go to the `provisioning/vagrant` folder and run `vagrant up`.
After some 10 minutes, the machine will be setup and running. Port forwarding is setup, so you should
be able to use the machine from your host. The web UI is available at
[http://localhost:10000](http://localhost:10000).

You can run `make simulate` from the root folder to generate some random data. The stats should start
appearing after some 10-20 seconds.


# What it is made up of

## Components

The starting point for all operations resides in module `cloak_metrics` which is in addition responsible
for periodic flushing of collected statistics.

Metrics are collected in `cloak_metrics_data` module which implements the corresponding data structure.
Internally, it relies on `cloak_metrics_data` and `cloak_metrics_aggregation` to collect and aggregate
histogram metrics. Counters are simple types, so they are maintained directly in
`cloak_metrics_data` module.

Data is dispatched to reporters via dispatchers. When `cloak_metrics` is started, you must provide a
list of reporters. For each reporter, a dedicated `cloak_metrics_dispatcher` powered process is started.
At the end of the collection interval, `cloak_metrics` first asks `cloak_metrics_data` to aggregate
everything, and then simply sends this to all dispatchers.

A reporter definition consists of an encoder and a transporter. The encoder is responsible for transforming
aggregated data to something that the target can accept. The transporter then simply must forward this data to
the target. Out of the box, `cloak_metrics_carbon_pickle` encoder is provided, together with
`cloak_metrics_tcp_transport` transporter. Both can be combined with the utility
`cloak_metrics:graphite_pickle_reporter/2` function to create a Graphite pickle reporter.

## Supervision tree

The top level supervisor is `cloak_metrics_sup` which is a `simple_one_for_one` supervisor responsible for
dynamic starting of the singleton `cloak_metrics` instance. Notice that worker server is not started
immediately. It is up to you to call `cloak_metrics:start_server/1` at some point thus starting the singleton
`cloak_metrics` instance which in turn starts dispatcher instances based on provided reporters.
