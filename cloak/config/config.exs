# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# turn off sasl error logger
config :sasl, :sasl_error_logger, false

config :logger,
  level: :info,
  backends: [:console]

config :kernel,
  # The portnumber the cloak erlang vm listens
  # to for communication from other nodes.
  inet_dist_listen_min: 34423,
  inet_dist_listen_max: 34423

config :os_mon,
  # Check RAM usage every minute, set alarm if more than 70% used.
  memory_check_interval: 1,
  system_memory_high_watermark: 0.7,
  # Check disk usage every minute, set alarm if more than 80% used.
  disk_space_check_interval: 1,
  disk_almost_full_threshold: 0.8

config :cloak, :api, port: 8098

config :cloak, :air,
  # The URL that results from asynchronous queries are sent to
  return_url: 'https://infrastructure-api.aircloak.com/results'

config :cloak, :queries,
  # The time to wait for the task coordinator to finish
  # the query. If it hasn't responded in that time, the
  # query times out completely, without any answers.
  query_timeout: 3600000, # 1 hour

  # The time we wait in synchronous queries for the task
  # to finish.  The timeout is set relatively high to allow
  # more complex tasks to be run synchronously without
  # having to pass through the web.
  # Once we have AirPub in place as a central dispatch
  # point for query results, we should reduce the
  # synchronous query timeout to reduce the number of
  # file descriptions potentially help open concurrently.
  sync_query_timeout: 3600000,

  # This is the number of concurrent task_coordinators that
  # are spawned per node.  If we run out of task_coordinators
  # the queued_worker mechanism queues tasks and runs them as
  # soon as a task_coordinator finished processing a task.
  concurrent_executions: 3

config :cloak, :noise,
  # The minimum number of users that must be in a bucket to get reported.
  absolute_lower_bound: 2,

  # After adding noise with standard deviation sigma_soft_lower_bound to the number of users in a bucket
  # this noisy count has to be greater than soft_lower_bound to get reported.
  sigma_soft_lower_bound: 1,
  soft_lower_bound: 5,

  # target_error: The target error for which the anonymization engine will add noise to the results.
  target_error: 0.01,

  # min_sigma: The minimum standard deviation allowed for noise.
  # max_sigma: The maximum total standard deviation to be used. This can be exceeded without
  # causing privacy issues, but is honoured to improve the usability of the system.
  min_sigma: 2,
  max_sigma: 20,

  # Percentile of users that will be discarded due to high-touch suppression. Percentile is taken
  # over the report ratio of each user, where a user's report ratio is reported_count/queried_count.
  high_touch_suppression_percentile: 0.00001,

  # Recalculation intervals for report ratio calculation. A recalculation interval is number
  # of times a user is queried, after which his report ratio is recalculated.
  high_touch_suppression_recalc_intervals: [100, 1000, 10000, 100000],

  # The anonymized results contain a layer of noise that is constant and unique to
  # the bucket. The noise is normal with a certain standard deviation.
  constant_noise_sd: 1.5

config :cloak, :metrics,
  # Graphite server and pickle port
  graphite_server: 'cloakmetrics.mpi-sws.org',
  graphite_port: 2004

config :cloak, :sandbox,
  # Number of ms a job is allowed to execute in the sandbox before being terminated
  max_time: 120000 # 2 minutes

config :cloak, :alarm_handler, install: true

config :cloak, :in_development, false


import_config "#{Mix.env}.exs"
