# Implemented Dataflow

## Task execution
  * `task_resource` --{task}--> `task_coordinator`
  * Task distribution across the entire cluster: (on a single node) `task_coordinator` --{task}--> (on each node) `task_local_runner`
  * `task_local_runner` uses `job_data_streamer` to retrieve input data information from the database and create the initial data stream states
  * Per-user concurrent sandbox execution: `task_local_runner` --> `job_runner` --> `job_data_streamer` -> {user_data} --> `sandbox`
  * When the job is finished, `job_runner` inserts all data into the database using `inserter` module
  * `job_runner` --{job_response}--> `task_local_runner` --{job_responses}--> `task_coordinator`
  * From there the results are sent to the global cache (`task_cache*`) which uses the `result_sanitizer` to sanitize the results, `aggregator` to update the global cache, `result_generation` to generate a result (which uses `anonymizer` and `range_generation` for the two steps of anonymization and the generation of ranges to report), and `result_sender` to report back the results to the air.

## Data insertion
  * `job_runner` or `insert_resource` or `bulk_insert_resource` --{user_data}--> `inserter`
  * `inserter` --{insert_job}--> `query_vnode` (on some nodes)
  * `query_vnode` --{insert_job}--> `insert_worker`

## Other modules of interest:
  * `cloak`/`cloak_app`:  basic application-level handling.
  * `cloak_conf`:  configuration management.
  * `cloak_distribution`:  providing normal distributed random variables.
  * `jvm_interface`:  interfacing the JVM (just a single function to send to the JVM Erlang node).
  * `preflists`:  functions for handling the riak_core preference lists.
