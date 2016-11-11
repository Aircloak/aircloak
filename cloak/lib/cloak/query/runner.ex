defmodule Cloak.Query.Runner do
  @moduledoc """
  Cloak query runner.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """

  use GenServer
  require Logger

  alias Cloak.Aql.{Query, Column, Comparison}
  alias Cloak.DataSource
  alias Cloak.Query.{Aggregator, LCFConditions, Sorter, Result, DataDecoder}

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
  @spec start(String.t, Cloak.DataSource.t, String.t, [any], Cloak.ResultSender.target) :: :ok
  def start(query_id, data_source, statement, parameters, result_target \\ :air_socket) do
    {:ok, _} = Supervisor.start_child(@supervisor_name,
      [{query_id, data_source, statement, parameters, result_target}, [name: worker_name(query_id)]])
    :ok
  end

  @spec stop(String.t) :: :ok
  def stop(query_id), do:
    query_id |> worker_name() |> GenServer.cast(:stop_query)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({query_id, data_source, statement, parameters, result_target}) do
    Process.flag(:trap_exit, true)
    {:ok, %{
      query_id: query_id,
      result_target: result_target,
      start_time: :erlang.monotonic_time(:milli_seconds),
      execution_time: nil,
      # We're starting the runner as a direct child.
      # This GenServer will wait for the runner to return or crash. Such approach allows us to
      # detect a failure no matter how the query fails (even if the runner process is for example killed).
      runner: Task.async(fn() -> run_query(data_source, statement, parameters) end)
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

  defp run_query(data_source, statement, parameters) do
    Logger.debug("Parsing statement `#{statement}` ...")
    with {:ok, sql_query} <- Query.make(data_source, statement, parameters) do
      execute_sql_query(sql_query)
    end
  end

  defp execute_sql_query(%Query{command: :show, show: :tables} = query) do
    query = %Query{query | columns: [%Column{table: :unknown, constant?: true, name: "name", type: :text}]}
    buckets = for {id, _table} <- query.data_source.tables, do: %{occurrences: 1, row: [id]}
    successful_result(
      %Result{columns: ["name"], buckets: buckets, features: Query.extract_features(query)},
      query
    )
  end
  defp execute_sql_query(%Query{command: :show, show: :columns} = query) do
    columns = ["name", "type"]
    query = %Query{query | columns: Enum.map(
      columns,
      &%Column{table: :unknown, constant?: true, name: &1, type: :text}
    )}
    [table] = query.selected_tables
    buckets = for {name, type} <- table.columns, do: %{occurrences: 1, row: [name, type]}
    successful_result(
      %Result{buckets: buckets, columns: columns, features: Query.extract_features(query)},
      query
    )
  end
  defp execute_sql_query(%Query{command: :select} = query) do
    try do
      with {:ok, result} <- select_rows(query) do
        result = %Result{result |
          columns: query.column_titles,
          features: Query.extract_features(query),
        }
        successful_result(result, query)
      end
    rescue e in [RuntimeError] ->
      {:error, e.message}
    end
  end

  defp select_rows(query), do:
    DataSource.select(query, fn(rows) ->
      Logger.debug("Processing rows ...")
      rows
      |> DataDecoder.decode(query)
      |> filter_on_encoded_columns(query)
      |> LCFConditions.apply(query)
      |> Aggregator.aggregate(query)
      |> Sorter.order(query)
      |> distinct(query)
      |> offset(query)
      |> limit(query)
    end)

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

  defp distinct(%Result{buckets: buckets} = result, %Query{distinct: true}), do:
    %Result{result | buckets: Enum.map(buckets, &Map.put(&1, :occurrences, 1))}
  defp distinct(result, %Query{distinct: false}), do: result

  defp filter_on_encoded_columns(stream, %Query{encoded_where: []}), do: stream
  defp filter_on_encoded_columns(stream, %Query{encoded_where: conditions}) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    Stream.filter(stream, &Enum.all?(filters, fn (filter) -> filter.(&1) end))
  end


  # -------------------------------------------------------------------
  # Test support
  # -------------------------------------------------------------------

  if Mix.env == :test do
    def run_sync(data_source, statement, parameters), do: run_query(data_source, statement, parameters)
    # tests run the same query in parallel, so we make the process name unique to avoid conflicts
    def worker_name(_query_id), do: {:via, :gproc, {:n, :l, {__MODULE__, :erlang.unique_integer()}}}
  else
    def worker_name(query_id), do: {:via, :gproc, {:n, :l, {__MODULE__, query_id}}}
  end
end
