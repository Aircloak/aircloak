defmodule Cloak.Query.Runner do
  @moduledoc """
  Cloak query runner.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """

  use GenServer
  require Logger

  alias Cloak.Aql.Query
  alias Cloak.DataSource
  alias Cloak.Query.{Aggregator, LCFConditions, Sorter, Result}

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
  @spec start(String.t, Cloak.DataSource.t, String.t Cloak.ResultSender.target) :: :ok
  def start(query_id, data_source, statement, result_target \\ :air_socket) do
    {:ok, _} = Supervisor.start_child(@supervisor_name, [{query_id, data_source, statement, result_target}])
    :ok
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({query_id, data_source, statement, result_target}) do
    Process.flag(:trap_exit, true)
    {:ok, %{
      query_id: query_id,
      result_target: result_target,
      start_time: :erlang.monotonic_time(:milli_seconds),
      execution_time: nil,
      # We're starting the runner as a direct child.
      # This GenServer will wait for the runner to return or crash. Such approach allows us to
      # detect a failure no matter how the query fails (even if the runner process is for example killed).
      runner: Task.async(fn() -> run_query(data_source, statement) end)
    }}
  end

  @doc false
  def handle_info({:EXIT, runner_pid, reason}, %{runner: %Task{pid: runner_pid}} = state) do
    if reason != :normal do
      report_result(state, {:error, "Cloak error"})
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


  ## ----------------------------------------------------------------
  ## Query runner
  ## ----------------------------------------------------------------

  defp run_query(data_source, statement) do
    Logger.debug("Parsing statement `#{statement}` ...")
    with {:ok, sql_query} <- Query.make(data_source, statement) do
      execute_sql_query(sql_query)
    end
  end

  defp execute_sql_query(%Query{command: :show, show: :tables} = query) do
    columns = ["name"]
    buckets = for {id, _table} <- query.data_source.tables, do: %{occurrences: 1, row: [id]}
    successful_result(%Result{buckets: buckets, columns: columns}, query)
  end
  defp execute_sql_query(%Query{command: :show, show: :columns} = query) do
    columns = ["name", "type"]
    [table] = query.selected_tables
    buckets = for {name, type} <- table.columns, do: %{occurrences: 1, row: [name, type]}
    successful_result(%Result{buckets: buckets, columns: columns}, query)
  end
  defp execute_sql_query(%Query{command: :select} = query) do
    try do
      with {:ok, result} <- DataSource.select(query, fn (rows) ->
        Logger.debug("Processing rows ...")
        rows
        |> LCFConditions.apply(query)
        |> Aggregator.aggregate(query)
        |> Sorter.order(query)
        |> offset(query)
        |> limit(query)
      end), do: successful_result(%Result{result | columns: query.column_titles}, query)
    rescue e in [RuntimeError] ->
      {:error, e.message}
    end
  end

  defp successful_result(result, query),
    do: {:ok, result, Enum.reverse(query.info)}


  # -------------------------------------------------------------------
  # Result reporting
  # -------------------------------------------------------------------

  defp report_result(state, {:ok, result, info}) do
    state = add_execution_time(state)
    log_completion(state, status: :success, row_count: length(result.buckets))
    result = %{
      columns: result.columns,
      rows: result.buckets,
      info: info,
      execution_time: execution_time_in_s(state),
      users_count: result.users_count,
    }
    send_result(state, result)
  end
  defp report_result(state, {:error, reason}) do
    state = add_execution_time(state)
    log_completion(state, status: :error, reason: reason)
    send_result(state, %{error: format_error_reason(reason), execution_time: execution_time_in_s(state)})
  end

  defp send_result(%{result_target: target, query_id: query_id}, partial_result) do
    Cloak.ResultSender.send_result(target, Map.put(partial_result, :query_id, query_id))
  end

  defp log_completion(state, options) do
    message = Poison.encode!(%{
      query_id: state.query_id,
      type: :query_complete,
      execution_time: state.execution_time,
      status: Keyword.get(options, :status),
      reason: Keyword.get(options, :reason, ""),
      row_count: Keyword.get(options, :row_count, 0),
    })

    Logger.info("JSON_LOG #{message}")
  end

  defp format_error_reason(text) when is_binary(text), do: text
  defp format_error_reason(reason) do
    Logger.error("Unknown query error: #{inspect(reason)}")
    "Cloak error"
  end

  defp add_execution_time(state) do
    %{state | execution_time: :erlang.monotonic_time(:milli_seconds) - state.start_time}
  end

  defp execution_time_in_s(%{execution_time: execution_time}) do
    div(execution_time, 1000)
  end

  defp limit(result, %Query{limit: nil}), do: result
  defp limit(%Result{buckets: buckets} = result, %Query{limit: amount}) do
    limited_buckets = buckets
      |> take(amount, [])
      |> Enum.reverse()
    %Result{result | buckets: limited_buckets}
  end

  defp take([], _amount, acc), do: acc
  defp take([%{occurrences: occurrences} = bucket | rest], amount, acc) when occurrences < amount, do:
    take(rest, amount - occurrences, [bucket | acc])
  defp take([%{} = bucket | _rest], amount, acc), do: [%{bucket | occurrences: amount} | acc]

  defp offset(%Result{buckets: buckets} = result, %Query{offset: amount}) do
    %Result{result | buckets: drop(buckets, amount)}
  end

  defp drop(buckets, 0), do: buckets
  defp drop([], _amount), do: []
  defp drop([%{occurrences: occurrences} | rest], amount) when occurrences <= amount, do:
    drop(rest, amount - occurrences)
  defp drop([%{occurrences: occurrences} = bucket | rest], amount), do:
    [%{bucket | occurrences: occurrences - amount} | rest]


  # -------------------------------------------------------------------
  # Test support
  # -------------------------------------------------------------------

  if Mix.env == :test do
    def run_sync(data_source, statement), do: run_query(data_source, statement)
  end
end
