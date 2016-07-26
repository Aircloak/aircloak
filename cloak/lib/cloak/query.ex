defmodule Cloak.Query do
  @moduledoc """
  Module for starting queries.

  This module implements the main server process which starts the queries concurrently,
  waits for it to respond, and sends the result back. If the query terminates abnormally,
  the server will send an error result back.
  """
  use GenServer
  require Logger

  alias Cloak.Query.Runner

  defstruct [:id, :statement, :data_source]

  @type t :: %__MODULE__{
    id: String.t,
    statement: String.t,
    data_source: Cloak.DataSource.t,
  }

  @supervisor_name Module.concat(__MODULE__, Supervisor)


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
  @spec start(t, Cloak.ResultSender.target) :: :ok
  def start(query, result_target \\ :air_socket) do
    {:ok, _} = Supervisor.start_child(@supervisor_name, [{query, result_target}])
    :ok
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({query, result_target}) do
    Process.flag(:trap_exit, true)
    {:ok, %{
      query_id: query.id,
      result_target: result_target,
      start_time: :erlang.monotonic_time(:milli_seconds),
      # We're starting the runner as a direct child.
      # This GenServer will wait for the runner to return or crash. Such approach allows us to
      # detect a failure no matter how the query fails (even if the runner process is for example killed).
      runner: Task.async(fn() -> Runner.run(query) end)
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
  def handle_info(msg, %{runner: runner} = state) do
    with {result, ^runner} <- Task.find([runner], msg) do
      report_result(state, result)
    end
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp report_result(state, {:ok, {:buckets, columns, rows}}) do
    log_completion(state, status: :success, row_count: length(rows))
    send_result(state, %{columns: columns, rows: rows})
  end
  defp report_result(state, {:error, reason}) do
    log_completion(state, status: :error, reason: reason)
    send_result(state, %{error: format_error_reason(reason)})
  end

  defp send_result(%{result_target: target, query_id: query_id}, partial_result) do
    Cloak.ResultSender.send_result(target, Map.put(partial_result, :query_id, query_id))
  end

  defp log_completion(state, options) do
    message = Poison.encode!(%{
      query_id: state.query_id,
      type: :query_complete,
      execution_time: :erlang.monotonic_time(:milli_seconds) - state.start_time,
      status: Keyword.get(options, :status),
      reason: Keyword.get(options, :reason, ""),
      row_count: Keyword.get(options, :row_count, 0),
    })

    Logger.info("JSON_LOG #{message}")
  end

  defp format_error_reason(text) when is_binary(text), do: text
  defp format_error_reason(%Postgrex.Error{} = error), do: Exception.message(error)
  defp format_error_reason(reason) do
    Logger.error("Unknown query error reason: #{inspect(reason)}")
    "Cloak error"
  end
end
