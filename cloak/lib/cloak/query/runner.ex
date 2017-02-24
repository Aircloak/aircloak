defmodule Cloak.Query.Runner do
  @moduledoc """
  Cloak query runner.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """

  use GenServer
  require Logger
  alias Cloak.{Sql.Query, DataSource, Query.Runner.Engine}

  @supervisor_name Module.concat(__MODULE__, Supervisor)
  @registry_name Module.concat(__MODULE__, Registry)

  defmodule RuntimeError do
    @moduledoc """
    An error that occurred while running the query.

    This error can be used to signal an error that will be caught by the runner
    and reported to the user.

    You're advised to not overuse this mechanism. However, sometimes it can be
    quite complicated to bubble up an error from a deep nested stack of maps,
    reduces, and other transformations. In such cases, you can raise this error
    with a descriptive message which will be reported to the end-user.
    """

    defexception [:message]
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the supervisor specification for the query runner supervisor."
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    import Supervisor.Spec, warn: false

    supervisor(Supervisor, [
      [
        supervisor(Registry, [:unique, @registry_name]),
        supervisor(Supervisor, [
          [worker(GenServer, [__MODULE__], restart: :temporary)],
          [id: @supervisor_name, name: @supervisor_name, strategy: :simple_one_for_one]
        ])
      ],
      [id: Module.concat(__MODULE__, TopLevelSupervisor), strategy: :rest_for_one]
    ])
  end

  @doc """
  Starts the query execution concurrently.

  This function returns as soon as the query runner process is started. The result
  is sent to the required destination. If an error occurs, the result will contain
  error information.
  """
  @spec start(String.t, DataSource.t, String.t, [DataSource.field], Query.view_map,
    Cloak.ResultSender.target) :: :ok
  def start(query_id, data_source, statement, parameters, views, result_target \\ :air_socket) do
    {:ok, _} = Supervisor.start_child(@supervisor_name,
      [{query_id, data_source, statement, parameters, views, result_target}, [name: worker_name(query_id)]])
    :ok
  end

  @spec stop(String.t | pid, :cancel | :oom) :: :ok
  def stop(query_pid, reason) when is_pid(query_pid), do:
    GenServer.cast(query_pid, {:stop_query, reason})
  def stop(query_id, reason), do:
    GenServer.cast(worker_name(query_id), {:stop_query, reason})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({query_id, data_source, statement, parameters, views, result_target}) do
    Logger.metadata(query_id: query_id)
    Process.flag(:trap_exit, true)
    cancel_callback = Cloak.MemoryReader.register_query()
    {:ok, %{
      query_id: query_id,
      result_target: result_target,
      start_time: :erlang.monotonic_time(:milli_seconds),
      execution_time: nil,
      # We're starting the runner as a direct child.
      # This GenServer will wait for the runner to return or crash. Such approach allows us to
      # detect a failure no matter how the query fails (even if the runner process is for example killed).
      runner: Task.async(fn() ->
        run_query(query_id, data_source, statement, parameters, views, cancel_callback)
      end)
    }}
  end

  @doc false
  def handle_info({:EXIT, runner_pid, reason}, %{runner: %Task{pid: runner_pid}} = state) do
    if reason != :normal do
      report_result(state, {:error, "Unknown cloak error."})
    end

    # Note: we're always exiting with a reason normal. If a query crashed, the error will be
    # properly logged, so no need to add more noise.
    {:stop, :normal, state}
  end
  def handle_info({runner_ref, result}, %{runner: %Task{ref: runner_ref}} = state) do
    report_result(state, result)
    {:noreply, state}
  end
  def handle_info(_other, state), do:
    {:noreply, state}

  def handle_cast({:stop_query, reason}, %{runner: task} = state) do
    Task.shutdown(task)
    case reason do
      :cancel -> report_result(state, {:error, "Cancelled."})
      :oom -> report_result(state, {:error, "Query aborted due to low memory."})
    end
    {:stop, :normal, %{state | runner: nil}}
  end


  ## ----------------------------------------------------------------
  ## Query runner
  ## ----------------------------------------------------------------

  defp run_query(query_id, data_source, statement, parameters, views, query_killing_cancel_callback) do
    Logger.metadata(query_id: query_id)
    Logger.debug("Parsing statement `#{statement}` ...")
    with {:ok, query} <- Query.make(data_source, statement, parameters, views),
         {:ok, result} <- Engine.run(query) do
      query_killing_cancel_callback.()
      {:ok, result, Query.info_messages(query)}
    end
  end


  # -------------------------------------------------------------------
  # Result reporting
  # -------------------------------------------------------------------

  defp report_result(state, result) do
    result =
      result
      |> format_result()
      |> Map.put(:query_id, state.query_id)
      |> Map.put(:execution_time, :erlang.monotonic_time(:milli_seconds) - state.start_time)
    log_completion(result)
    # send execution time in seconds, to avoid timing attacks
    result = %{result | execution_time: div(result.execution_time, 1000)}
    Cloak.ResultSender.send_result(state.result_target, result)
  end

  defp log_completion(result) do
    message = Poison.encode!(%{
      query_id: result.query_id,
      type: :query_complete,
      execution_time: result.execution_time,
      status: result[:error] || "Successful.",
    })

    Logger.info("JSON_LOG #{message}")
  end

  defp format_result({:ok, result, info}), do:
    %{
      columns: result.columns,
      rows: result.buckets,
      info: info,
      users_count: result.users_count,
      features: result.features,
    }
  defp format_result({:error, reason}) when is_binary(reason), do:
    %{error: reason}
  defp format_result({:error, reason}) do
    Logger.error("Unknown query error: #{inspect(reason)}")
    format_result({:error, "Unknown cloak error."})
  end


  # -------------------------------------------------------------------
  # Test support
  # -------------------------------------------------------------------

  if Mix.env == :test do
    # tests run the same query in parallel, so we make the process name unique to avoid conflicts
    def worker_name(_query_id), do: {:via, Registry, {@registry_name, :erlang.unique_integer()}}
  else
    def worker_name(query_id), do: {:via, Registry, {@registry_name, query_id}}
  end
end
