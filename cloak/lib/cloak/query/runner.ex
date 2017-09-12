defmodule Cloak.Query.Runner do
  @moduledoc """
  Cloak query runner.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """

  use GenServer
  require Logger
  alias Cloak.{Sql.Query, DataSource, Query.Runner.Engine, ResultSender}

  @supervisor_name __MODULE__.Supervisor
  @runner_registry_name __MODULE__.RunnerRegistry
  @queries_registry_name __MODULE__.QueriesRegistry


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the supervisor specification for the query runner supervisor."
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    import Supervisor.Spec, warn: false

    supervisor(Supervisor, [
      [
        supervisor(Registry, [:unique, @runner_registry_name], id: @runner_registry_name),
        supervisor(Registry, [:unique, @queries_registry_name], id: @queries_registry_name),
        supervisor(Supervisor, [
          [worker(GenServer, [__MODULE__], restart: :temporary)],
          [id: @supervisor_name, name: @supervisor_name, strategy: :simple_one_for_one]
        ])
      ],
      [id: __MODULE__.TopLevelSupervisor, strategy: :rest_for_one]
    ])
  end

  @doc """
  Starts the query execution concurrently.

  This function returns as soon as the query runner process is started. The result
  is sent to the required destination. If an error occurs, the result will contain
  error information.
  """
  @spec start(String.t, DataSource.t, String.t, [DataSource.field], Query.view_map, ResultSender.target) :: :ok
  def start(query_id, data_source, statement, parameters, views, result_target \\ :air_socket) do
    {:ok, _} = Supervisor.start_child(@supervisor_name,
      [{query_id, data_source, statement, parameters, views, result_target}, [name: worker_name(query_id)]])
    :ok
  end

  @spec stop(String.t | pid, :cancelled | :oom) :: :ok
  def stop(query_pid, reason) when is_pid(query_pid), do:
    GenServer.cast(query_pid, {:stop_query, reason})
  def stop(query_id, reason), do:
    GenServer.cast(worker_name(query_id), {:stop_query, reason})

  @doc "Returns true if the worker for the given query is still alive, false otherwise."
  @spec alive?(String.t) :: boolean
  def alive?(query_id) do
    case Registry.lookup(@runner_registry_name, query_id) do
      [] -> false
      [_ | _] -> true
    end
  end

  @doc "Returns the list of ids of running queries."
  @spec running_queries() :: [String.t]
  def running_queries(), do:
    Enum.map(
      Registry.lookup(@queries_registry_name, :instances),
      fn({_pid, query_id}) -> query_id end
    )

  @doc "Executes the query synchronously, and returns its result."
  @spec run_sync(String.t, DataSource.t, String.t, [DataSource.field], Query.view_map) :: any
  def run_sync(query_id, data_source, statement, parameters, views) do
    start(query_id, data_source, statement, parameters, views, {:process, self()})
    receive do
      {:result, response} -> response
    end
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({query_id, data_source, statement, parameters, views, result_target}) do
    Registry.register(@queries_registry_name, :instances, query_id)
    Logger.metadata(query_id: query_id)
    Process.flag(:trap_exit, true)
    owner = self()
    memory_callbacks = Cloak.MemoryReader.query_registering_callbacks()
    {:ok, %{
      query_id: query_id,
      result_target: result_target,
      start_time: :erlang.monotonic_time(:milli_seconds),
      execution_time: nil,
      features: nil,
      # We're starting the runner as a direct child.
      # This GenServer will wait for the runner to return or crash. Such approach allows us to
      # detect a failure no matter how the query fails (even if the runner process is for example killed).
      runner: Task.async(fn() ->
        run_query(query_id, owner, data_source, statement, parameters, views, memory_callbacks)
      end),
    }}
  end

  @doc false
  def handle_info({:EXIT, runner_pid, reason}, %{runner: %Task{pid: runner_pid}} = state) do
    state =
      if reason != :normal do
        send_result_report(state, {:error, "Unknown cloak error."})
      else
        state
      end

    # Note: we're always exiting with a reason normal. If a query crashed, the error will be
    # properly logged, so no need to add more noise.
    {:stop, :normal, state}
  end
  def handle_info({:send_state, query_id, query_state}, state) do
    ResultSender.send_state(state.result_target, query_id, query_state)
    {:noreply, state}
  end
  def handle_info({:features, features}, state) do
    {:noreply, %{state | features: features}}
  end
  def handle_info({runner_ref, result}, %{runner: %Task{ref: runner_ref}} = state), do:
    {:noreply, send_result_report(state, result)}
  def handle_info(_other, state), do:
    {:noreply, state}

  @doc false
  def handle_cast({:stop_query, reason}, %{runner: task} = state) do
    Task.shutdown(task)
    Logger.warn("Asked to stop query. Reason: #{inspect reason}")
    {:stop, :normal, send_result_report(%{state | runner: nil}, reason)}
  end


  # -------------------------------------------------------------------
  # Query running
  # -------------------------------------------------------------------

  defp run_query(query_id, owner, data_source, statement, parameters, views, memory_callbacks) do
    Logger.metadata(query_id: query_id)
    Logger.debug("Running statement `#{statement}` ...")

    Engine.run(data_source, statement, parameters, views,
      _state_updater = &send(owner, {:send_state, query_id, &1}),
      _feature_updater = &send(owner, {:features, &1}),
      memory_callbacks)
  end


  # -------------------------------------------------------------------
  # Result reporting
  # -------------------------------------------------------------------

  defp send_result_report(state, result) do
    result =
      result
      |> format_result(state)
      |> Map.put(:query_id, state.query_id)
      |> Map.put(:execution_time, :erlang.monotonic_time(:milli_seconds) - state.start_time)

    log_completion(result)

    ResultSender.send_result(state.result_target, %{result | execution_time: timing_attack_safe(result.execution_time)})
    |> case do
      {:error, :encoding_error} -> send_result_report(state, {:error, "Encoding error"})
      _ -> state
    end
  end

  defp timing_attack_safe(execution_time), do:
    div(execution_time, _millis_in_second = 1000)

  defp log_completion(result) do
    message = Poison.encode!(%{
      query_id: result.query_id,
      type: :query_complete,
      execution_time: result.execution_time,
      status: result[:error] || "Successful.",
    })

    Logger.info("JSON_LOG #{message}")
  end

  defp format_result({:ok, result, info}, _state), do:
    %{
      columns: result.columns,
      rows: result.buckets,
      info: info,
      users_count: result.users_count,
      features: result.features,
    }
  defp format_result({:error, reason}, state) when is_binary(reason), do:
    %{error: reason, features: state[:features]}
  defp format_result(:oom, state), do:
    %{error: "Query aborted due to low memory.", features: state[:features]}
  defp format_result(:cancelled, state), do:
    %{cancelled: true, features: state[:features]}
  defp format_result({:error, reason}, state) do
    Logger.error("Unknown query error: #{inspect(reason)}")
    format_result({:error, "Unknown cloak error."}, state)
  end


  # -------------------------------------------------------------------
  # Test support
  # -------------------------------------------------------------------

  if Mix.env == :test do
    # tests run the same query in parallel, so we make the process name unique to avoid conflicts
    def worker_name(_query_id), do: {:via, Registry, {@runner_registry_name, :erlang.unique_integer()}}
  else
    def worker_name(query_id), do: {:via, Registry, {@runner_registry_name, query_id}}
  end
end
