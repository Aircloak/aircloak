defmodule Cloak.Query.Runner do
  @moduledoc """
  Cloak query runner.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """

  use GenServer
  require Logger
  alias Cloak.{Aql.Query, DataSource, Query.Result, Query.Select}

  @supervisor_name Module.concat(__MODULE__, Supervisor)

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
      [worker(GenServer, [__MODULE__], restart: :temporary)],
      [id: @supervisor_name, name: @supervisor_name, strategy: :simple_one_for_one]
    ])
  end

  @doc """
  Starts the query execution concurrently.

  This function returns as soon as the query runner process is started. The result
  is sent to the required destination. If an error occurs, the result will contain
  error information.
  """
  @lint {Credo.Check.Refactor.FunctionArity, false}
  @spec start(String.t, DataSource.t, String.t, [DataSource.field], Query.view_map,
    Cloak.ResultSender.target) :: :ok
  def start(query_id, data_source, statement, parameters, views, result_target \\ :air_socket) do
    {:ok, _} = Supervisor.start_child(@supervisor_name,
      [{query_id, data_source, statement, parameters, views, result_target}, [name: worker_name(query_id)]])
    :ok
  end

  @spec stop(String.t) :: :ok
  def stop(query_id), do:
    query_id |> worker_name() |> GenServer.cast(:stop_query)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({query_id, data_source, statement, parameters, views, result_target}) do
    Process.flag(:trap_exit, true)
    {:ok, %{
      query_id: query_id,
      result_target: result_target,
      start_time: :erlang.monotonic_time(:milli_seconds),
      execution_time: nil,
      # We're starting the runner as a direct child.
      # This GenServer will wait for the runner to return or crash. Such approach allows us to
      # detect a failure no matter how the query fails (even if the runner process is for example killed).
      runner: Task.async(fn() -> run_query(data_source, statement, parameters, views) end)
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

  def handle_cast(:stop_query, %{runner: task} = state) do
    Task.shutdown(task)
    report_result(state, {:error, "Cancelled."})
    {:stop, :normal, %{state | runner: nil}}
  end


  ## ----------------------------------------------------------------
  ## Query runner
  ## ----------------------------------------------------------------

  defp run_query(data_source, statement, parameters, views) do
    Logger.debug("Parsing statement `#{statement}` ...")
    with {:ok, sql_query} <- Query.make(data_source, statement, parameters, views) do
      execute_sql_query(sql_query)
    end
  end

  defp execute_sql_query(%Query{command: :show, show: :tables} = query) do
    buckets =
      Map.keys(query.data_source.tables) ++ Map.keys(query.views)
      |> Enum.map(&%{occurrences: 1, row: [to_string(&1)]})

    successful_result(
      %Result{columns: query.column_titles, buckets: buckets, features: Query.extract_features(query)},
      query
    )
  end
  defp execute_sql_query(%Query{command: :show, show: :columns} = query) do
    [table] = query.selected_tables
    buckets = for {name, type} <- table.columns, do: %{occurrences: 1, row: [name, type]}
    successful_result(
      %Result{buckets: buckets, columns: query.column_titles, features: Query.extract_features(query)},
      query
    )
  end
  defp execute_sql_query(%Query{command: :select} = query) do
    with {:ok, result} <- Select.run(query), do:
      successful_result(%Result{result | features: Query.extract_features(query)}, query)
  end

  defp successful_result(result, query), do: {:ok, result, Enum.reverse(query.info)}


  # -------------------------------------------------------------------
  # Result reporting
  # -------------------------------------------------------------------

  defp report_result(state, result) do
    result =
      result
      |> format_result()
      |> Map.put(:query_id, state.query_id)
      |> Map.put(:execution_time, execution_time_in_seconds(state))
    log_completion(result)
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

  defp execution_time_in_seconds(state), do:
    div(:erlang.monotonic_time(:milli_seconds) - state.start_time, 1000)


  # -------------------------------------------------------------------
  # Test support
  # -------------------------------------------------------------------

  if Mix.env == :test do
    def run_sync(data_source, statement, parameters, views), do:
      run_query(data_source, statement, parameters, views)

    # tests run the same query in parallel, so we make the process name unique to avoid conflicts
    def worker_name(_query_id), do: {:via, :gproc, {:n, :l, {__MODULE__, :erlang.unique_integer()}}}
  else
    def worker_name(query_id), do: {:via, :gproc, {:n, :l, {__MODULE__, query_id}}}
  end
end
