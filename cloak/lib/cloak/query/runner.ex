defmodule Cloak.Query.Runner do
  @moduledoc """
  Cloak query runner.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """

  use GenServer
  require Logger
  alias Aircloak.ChildSpec
  alias Cloak.{Sql.Query, DataSource, Query.Runner.Engine, ResultSender}

  @supervisor_name __MODULE__.Supervisor
  @runner_registry_name __MODULE__.RunnerRegistry
  @queries_registry_name __MODULE__.QueriesRegistry

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
          DataSource.t(),
          String.t(),
          [DataSource.field()],
          Query.view_map(),
          ResultSender.target()
        ) :: :ok
  def start(query_id, data_source, statement, parameters, views, result_target \\ :air_socket) do
    {:ok, _} =
      DynamicSupervisor.start_child(
        @supervisor_name,
        ChildSpec.gen_server(
          __MODULE__,
          {query_id, data_source, statement, parameters, views, result_target},
          [name: worker_name(query_id)],
          restart: :temporary
        )
      )

    :ok
  end

  @spec stop(String.t() | pid, :cancelled | :oom) :: :ok
  def stop(query_pid, reason) when is_pid(query_pid), do: GenServer.cast(query_pid, {:stop_query, reason})

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
  @spec run_sync(String.t(), DataSource.t(), String.t(), [DataSource.field()], Query.view_map()) :: any
  def run_sync(query_id, data_source, statement, parameters, views) do
    start(query_id, data_source, statement, parameters, views, {:process, self()})

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
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({query_id, data_source, statement, parameters, views, result_target}) do
    {:ok, _pid} = Registry.register(@queries_registry_name, :instances, query_id)
    Logger.metadata(query_id: query_id)
    Process.flag(:trap_exit, true)
    owner = self()
    memory_callbacks = Cloak.MemoryReader.query_registering_callbacks()

    {:ok,
     %{
       query_id: query_id,
       result_target: result_target,
       start_time: :erlang.monotonic_time(:milli_seconds),
       execution_time: nil,
       features: nil,
       log_format: Logger.Formatter.compile(Application.get_env(:logger, :console)[:format]),
       log_metadata: Application.get_env(:logger, :console)[:metadata],
       log: [],
       # We're starting the runner as a direct child.
       # This GenServer will wait for the runner to return or crash. Such approach allows us to
       # detect a failure no matter how the query fails (even if the runner process is for example killed).
       runner:
         Task.async(fn ->
           run_query(query_id, owner, data_source, statement, parameters, views, memory_callbacks)
         end)
     }}
  end

  @impl GenServer
  def handle_info({:EXIT, runner_pid, reason}, %{runner: %Task{pid: runner_pid}} = state) do
    state =
      if reason != :normal do
        state
        |> update_in([:log], &[&1, crash_log(reason), ?\n])
        |> send_result_report({:error, "Unknown cloak error."})
      else
        state
      end

    # Note: we're always exiting with a reason normal. If a query crashed, the error will be
    # properly logged, so no need to add more noise.
    {:stop, :normal, state}
  end

  def handle_info({:send_state, query_id, query_state}, state) do
    Logger.debug(fn -> "Query #{query_id} state changed to: #{query_state}..." end)
    ResultSender.send_state(state.result_target, query_id, query_state)
    {:noreply, state}
  end

  def handle_info({:features, features}, state) do
    {:noreply, %{state | features: features}}
  end

  def handle_info({runner_ref, result}, %{runner: %Task{ref: runner_ref}} = state),
    do: {:noreply, send_result_report(state, result)}

  def handle_info({:send_log_entry, level, message, timestamp, metadata}, state) do
    {:noreply, add_log_entry(state, level, message, timestamp, metadata)}
  end

  def handle_info(_other, state), do: {:noreply, state}

  @impl GenServer
  def handle_cast({:stop_query, reason}, %{runner: task} = state) do
    Task.shutdown(task)
    Logger.warn("Asked to stop query. Reason: #{inspect(reason)}")
    {:stop, :normal, send_result_report(%{state | runner: nil}, reason)}
  end

  # -------------------------------------------------------------------
  # Query running
  # -------------------------------------------------------------------

  defp run_query(query_id, owner, data_source, statement, parameters, views, memory_callbacks) do
    Logger.metadata(query_id: query_id)
    Logger.debug(fn -> "Running statement `#{statement}` ..." end)

    Engine.run(
      data_source,
      statement,
      parameters,
      views,
      _state_updater = &send(owner, {:send_state, query_id, &1}),
      _feature_updater = &send(owner, {:features, &1}),
      memory_callbacks
    )
  end

  # -------------------------------------------------------------------
  # Result reporting
  # -------------------------------------------------------------------

  defp add_log_entry(state, level, message, timestamp, metadata) do
    metadata = metadata |> Keyword.take(state.log_metadata) |> Keyword.delete(:query_id)

    update_in(
      state.log,
      &[&1, Logger.Formatter.format(state.log_format, level, message, timestamp, metadata)]
    )
  end

  defp crash_log({_exit_reason, stacktrace}) when is_list(stacktrace) do
    Exception.format_exit({"filtered exit reason", Cloak.LoggerTranslator.filtered_stacktrace(stacktrace)})
  end

  defp crash_log(_other_reason), do: "query process crashed"

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
      Poison.encode!(%{
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

  defp format_result({:error, reason}, state) when is_binary(reason), do: %{error: reason, features: state[:features]}

  defp format_result(:oom, state), do: %{error: "Query aborted due to low memory.", features: state[:features]}

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
        ChildSpec.registry(:unique, @runner_registry_name),
        ChildSpec.registry(:duplicate, @queries_registry_name),
        ChildSpec.dynamic_supervisor(name: @supervisor_name)
      ],
      strategy: :rest_for_one
    )
  end

  # -------------------------------------------------------------------
  # Test support
  # -------------------------------------------------------------------

  if Mix.env() == :test do
    # tests run the same query in parallel, so we make the process name unique to avoid conflicts
    def worker_name(_query_id), do: {:via, Registry, {@runner_registry_name, :erlang.unique_integer()}}
  else
    def worker_name(query_id), do: {:via, Registry, {@runner_registry_name, query_id}}
  end
end
