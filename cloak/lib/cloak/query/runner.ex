defmodule Cloak.Query.Runner do
  @moduledoc """
  Cloak query runner.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """

  use Parent.GenServer
  require Logger
  require Aircloak
  require Aircloak.DeployConfig
  alias Aircloak.ChildSpec
  alias Cloak.{Sql.Query, DataSource, Query.Runner.Engine, ResultSender}

  @supervisor_name __MODULE__.Supervisor
  @runner_registry_name __MODULE__.RunnerRegistry
  @queries_registry_name __MODULE__.QueriesRegistry

  @type start_opts :: [result_target: :air_socket | pid()]

  @type args :: %{
          query_id: String.t(),
          analyst_id: Query.analyst_id(),
          data_source: Cloak.DataSource.t(),
          statement: String.t(),
          parameters: [Cloak.DataSource.field()],
          views: [Cloak.Sql.Query.view_map()],
          state_updater: (Cloak.ResultSender.query_state() -> any),
          feature_updater: (Cloak.Query.features() -> any),
          memory_callbacks: Cloak.MemoryReader.query_killer_callbacks()
        }

  @type result :: {:ok, Sql.Query.Result.t(), [String.t()]} | {:error, String.t()}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the query execution concurrently.

  This function returns as soon as the query runner process is started. The result
  is sent to the required destination. If an error occurs, the result will contain
  error information.
  """
  @spec start(
          String.t(),
          Query.analyst_id(),
          DataSource.t(),
          String.t(),
          [DataSource.field()],
          Query.view_map(),
          start_opts
        ) :: :ok | {:error, :too_many_queries}
  def start(query_id, analyst_id, data_source, statement, parameters, views, start_opts \\ []) do
    runner_args =
      start_opts
      |> Map.new()
      |> Map.merge(%{
        query_id: query_id,
        analyst_id: analyst_id,
        data_source: data_source,
        statement: statement,
        parameters: parameters,
        views: views
      })

    # Starting of a query is serialized (queries are started one at a time). This makes it possible to reliably decide
    # if we can start the new query with respect to max concurrency settings.
    :jobs.run(__MODULE__, fn -> serialized_start_runner(query_id, runner_args) end)
  end

  @spec stop(String.t() | pid, :cancelled | :oom) :: :ok
  def stop(query_pid, reason) when is_pid(query_pid),
    do: GenServer.cast(query_pid, {:stop_query, reason})

  def stop(query_id, reason), do: GenServer.cast(worker_name(query_id), {:stop_query, reason})

  @doc "Returns true if the worker for the given query is still alive, false otherwise."
  @spec alive?(String.t()) :: boolean
  def alive?(query_id) do
    case Registry.lookup(@runner_registry_name, query_id) do
      [] -> false
      [_ | _] -> true
    end
  end

  @doc "Returns the list of ids of running queries."
  @spec running_queries() :: [String.t()]
  def running_queries(),
    do:
      Enum.map(Registry.lookup(@queries_registry_name, :instances), fn {_pid, query_id} ->
        query_id
      end)

  @doc "Executes the query synchronously, and returns its result."
  @spec run_sync(String.t(), Query.analyst_id(), DataSource.t(), String.t(), [DataSource.field()], Query.view_map()) ::
          any
  def run_sync(query_id, analyst_id, data_source, statement, parameters, views) do
    :ok = start(query_id, analyst_id, data_source, statement, parameters, views, result_target: self())

    receive do
      {:result, response} -> response
    end
  end

  @doc "Sends the query-specific log entry to the query runner."
  @spec send_log_entry(
          String.t(),
          Logger.level(),
          Logger.message(),
          Logger.Formatter.time(),
          Keyword.t()
        ) :: :ok
  def send_log_entry(query_id, level, message, timestamp, metadata) do
    {:via, Registry, {registry_name, registered_alias}} = worker_name(query_id)

    case Registry.lookup(registry_name, registered_alias) do
      [{pid, _}] -> send(pid, {:send_log_entry, level, message, timestamp, metadata})
      _ -> :ok
    end
  end

  # -------------------------------------------------------------------
  # Server starting
  # -------------------------------------------------------------------

  defp setup_queue() do
    with :undefined <- :jobs.queue_info(__MODULE__),
         do:
           :jobs.add_queue(__MODULE__,
             max_time: :timer.minutes(1),
             regulators: [counter: [limit: 1]]
           )
  end

  defp serialized_start_runner(query_id, runner_args) do
    if Registry.count(@runner_registry_name) < max_parallel_queries() do
      {:ok, _pid} = DynamicSupervisor.start_child(@supervisor_name, runner_spec(query_id, runner_args))

      :ok
    else
      {:error, :too_many_queries}
    end
  end

  defp max_parallel_queries() do
    Aircloak.in_env(
      test: Application.get_env(:cloak, :max_parallel_queries, :infinity),
      else: Aircloak.DeployConfig.get("max_parallel_queries", 10)
    )
  end

  defp runner_spec(query_id, runner_args) do
    %{
      id: __MODULE__,
      start: {Parent.GenServer, :start_link, [__MODULE__, runner_args, [name: worker_name(query_id)]]},
      restart: :temporary
    }
  end

  @doc false
  def worker_name(query_id) do
    import Aircloak, only: [in_env: 1, unused: 2]
    unused(query_id, in: [:test])

    in_env(
      test: {:via, Registry, {@runner_registry_name, :erlang.unique_integer()}},
      else: {:via, Registry, {@runner_registry_name, query_id}}
    )
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(runner_args) do
    {:ok, _pid} = Registry.register(@queries_registry_name, :instances, runner_args.query_id)
    Logger.metadata(query_id: runner_args.query_id)

    {runner_fun, runner_args} = Map.pop(runner_args, :runner_fun, &Engine.run/1)

    runner_args =
      Map.merge(runner_args, %{
        memory_callbacks: Cloak.MemoryReader.query_registering_callbacks(),
        state_updater: with(me <- self(), do: &send(me, {:send_state, runner_args.query_id, &1})),
        feature_updater: with(me <- self(), do: &send(me, {:features, &1}))
      })

    Parent.GenServer.start_child(%{
      id: :query_execution,
      start: with(me <- self(), do: {Task, :start_link, [fn -> run_query(runner_args, runner_fun, me) end]}),
      shutdown: :brutal_kill
    })

    {:ok,
     %{
       query_id: runner_args.query_id,
       result_target: Map.get(runner_args, :result_target, :air_socket),
       start_time: :erlang.monotonic_time(:milli_seconds),
       execution_time: nil,
       features: nil,
       log_format: Logger.Formatter.compile(Application.get_env(:logger, :console)[:format]),
       log_metadata: Application.get_env(:logger, :console)[:metadata],
       log: [],
       processing_steps: 1,
       query_state: nil
     }}
  end

  @impl GenServer
  # Some queries can have multiple anonymization steps, so the state sequence
  # `[:ingesting_data, :processing, :post_processing]` can arrive multiple times.
  # Joins are also executed in parallel, so it means this state sequence can also come out of order.
  # We want to aggregate the multiple, concurrent arrivals of this sequence into a global query state, as follows:
  #   - when the first `:ingesting_data` state arrives, we go into the `:ingesting_data` state.
  #   - when the first `:processing` state arrives, we go into the `:processing` state and stay there until
  #     all the other `processing` steps also arrive.
  #   - after all the `:processing` states have arrived and when the first `:post_processing` state arrives,
  #     we go into the `:post_processing` state.
  # The number of possible `:processing` steps in set in separate call, using `{:set_processing_steps, count}`.

  def handle_info({:send_state, _query_id, {:set_processing_steps, iterations}}, state),
    do: {:noreply, %{state | processing_steps: iterations}}

  def handle_info(
        {:send_state, _query_id, :processing},
        %{query_state: :processing, processing_steps: iterations} = state
      )
      when iterations > 1,
      do: {:noreply, %{state | processing_steps: iterations - 1}}

  def handle_info(
        {:send_state, _query_id, _ignored_state},
        %{query_state: :processing, processing_steps: iterations} = state
      )
      when iterations > 1,
      do: {:noreply, state}

  def handle_info({:send_state, _query_id, query_state}, %{query_state: query_state} = state),
    do: {:noreply, state}

  def handle_info({:send_state, query_id, query_state}, state) do
    Logger.debug(fn -> "Query #{query_id} state changed to: #{query_state}..." end)
    ResultSender.send_state(state.result_target, query_id, query_state)
    {:noreply, %{state | query_state: query_state}}
  end

  def handle_info({:features, features}, state), do: {:noreply, %{state | features: features}}

  def handle_info({:query_result, result}, state),
    do: {:noreply, send_result_report(state, result)}

  def handle_info({:send_log_entry, level, message, timestamp, metadata}, state) do
    {:noreply, add_log_entry(state, level, message, timestamp, metadata)}
  end

  @impl GenServer
  def handle_cast({:stop_query, reason}, state) do
    Parent.GenServer.shutdown_child(:query_execution)
    Logger.warn("Asked to stop query. Reason: #{inspect(reason)}")
    {:stop, :normal, send_result_report(state, reason)}
  end

  @impl Parent.GenServer
  def handle_child_terminated(:query_execution, _meta, _pid, reason, state) do
    state =
      if reason != :normal do
        state
        |> update_in([:log], &[&1, Cloak.LoggerTranslator.format_exit(reason), ?\n])
        |> send_result_report({:error, "Unknown cloak error."})
      else
        state
      end

    # Note: we're always exiting with a reason normal. If a query crashed, the error will be
    # properly logged, so no need to add more noise.
    {:stop, :normal, state}
  end

  # -------------------------------------------------------------------
  # Query running
  # -------------------------------------------------------------------

  @spec run_query(args, (args -> result), pid()) :: :ok
  defp run_query(runner_args, runner_fun, parent) do
    Logger.metadata(query_id: runner_args.query_id)
    Logger.debug(fn -> "Running statement `#{runner_args.statement}` ..." end)

    result = runner_fun.(runner_args)
    send(parent, {:query_result, result})
    :ok
  end

  # -------------------------------------------------------------------
  # Result reporting
  # -------------------------------------------------------------------

  defp add_log_entry(state, level, message, timestamp, metadata) do
    metadata = metadata |> Keyword.take(state.log_metadata) |> Keyword.delete(:query_id)

    update_in(
      state.log,
      &[
        &1,
        Logger.Formatter.format(
          state.log_format,
          level,
          message,
          Cloak.Time.truncate(timestamp, :second),
          metadata
        )
      ]
    )
  end

  defp send_result_report(state, result) do
    result =
      result
      |> format_result(state)
      |> Map.put(:query_id, state.query_id)
      |> Map.put(:execution_time, :erlang.monotonic_time(:milli_seconds) - state.start_time)

    log_completion(result)
    state = flush_log_messages(state)
    result = Map.put(result, :log, to_string(state.log))

    ResultSender.send_result(state.result_target, %{
      result
      | execution_time: timing_attack_safe(result.execution_time)
    })
    |> case do
      {:error, :encoding_error} -> send_result_report(state, {:error, "Encoding error"})
      _ -> state
    end
  end

  defp timing_attack_safe(execution_time), do: div(execution_time, _millis_in_second = 1000)

  defp log_completion(result) do
    message =
      Jason.encode!(%{
        query_id: result.query_id,
        type: :query_complete,
        execution_time: result.execution_time,
        status: result[:error] || "Successful."
      })

    Logger.info("JSON_LOG #{message}")
  end

  defp flush_log_messages(state) do
    # producing one final log message, so we know when to stop flushing messages
    Logger.info("query finished")
    do_flush_log_messages(state)
  end

  defp do_flush_log_messages(state) do
    # We're trying to flush all the log messages, stopping at the very last message logged in flush_log_messages/1.
    receive do
      {:send_log_entry, level, message, timestamp, metadata} ->
        message = to_string(message)
        state = add_log_entry(state, level, message, timestamp, metadata)

        if String.starts_with?(message, "query finished"),
          do: state,
          else: do_flush_log_messages(state)
    after
      Application.fetch_env!(:cloak, :flush_query_log_timeout) ->
        # To avoid blocking the query execution for too long, we'll timeout quickly if no new log messages arrive.
        state
    end
  end

  defp format_result({:ok, result, info}, _state),
    do: %{
      columns: result.columns,
      rows: result.buckets,
      info: info,
      features: result.features
    }

  defp format_result({:error, reason}, state) when is_binary(reason),
    do: %{error: reason, features: state[:features]}

  defp format_result(:oom, state),
    do: %{error: "Query aborted due to low memory.", features: state[:features]}

  defp format_result(:cancelled, state), do: %{cancelled: true, features: state[:features]}

  defp format_result({:error, reason}, state) do
    Logger.error("Unknown query error: #{inspect(reason)}")
    format_result({:error, "Unknown cloak error."}, state)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        ChildSpec.setup_job(&setup_queue/0),
        ChildSpec.registry(:unique, @runner_registry_name),
        ChildSpec.registry(:duplicate, @queries_registry_name),
        ChildSpec.dynamic_supervisor(name: @supervisor_name)
      ],
      strategy: :rest_for_one,
      name: __MODULE__
    )
  end
end
