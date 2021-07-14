defmodule Air.Service.Logs do
  @moduledoc "Service for logs management."

  alias Air.Repo
  alias Air.Schemas.Log
  import Ecto.Query
  require Logger

  use GenServer

  @flush_interval 1000
  @high_load_threshold 100
  @high_load_timeout_max 5

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Queues a new log entry for saving into the database."
  @spec save(NaiveDateTime.t(), Log.Source.t(), String.t(), Log.Level.t(), String.t()) :: :ok
  def save(timestamp, source, hostname, level, message) do
    log_entry = %{timestamp: timestamp, source: source, hostname: hostname, level: level, message: message}
    GenServer.cast(__MODULE__, {:store, log_entry})
  end

  @doc "Flushes all buffered log entries to database."
  @spec flush() :: :ok
  def flush() do
    GenServer.call(__MODULE__, :flush_once)
  end

  @doc "Returns the most recent log entries, sorted in ascendent order by timestamp."
  @spec tail(Map.t(), pos_integer()) :: [Log.t()]
  def tail(filters, max_entries) do
    Log
    |> filter_by_id(filters)
    |> filter_by_timestamp(filters)
    |> filter_by_level(filters)
    |> filter_by_source(filters)
    |> order_by([log], desc: log.timestamp)
    |> limit(^max_entries)
    |> Repo.all()
    |> Enum.reverse()
  end

  @doc "Streams all log entries, sorted in ascendent order by timestamp."
  @spec stream_all((Enum.t() -> any)) :: any
  def stream_all(stream_handler) do
    stream = Log |> order_by([log], [log.timestamp, log.id]) |> Repo.stream()
    {:ok, result} = Repo.transaction(fn -> stream_handler.(stream) end, timeout: :timer.hours(1))
    result
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  def start_link(_args), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  @impl GenServer
  def init(_) do
    # prevent recursion from additional log messages
    Logger.disable(self())
    start_log_collection()
    schedule_flush()
    {:ok, %{logs: [], logs_count: 0, high_load_timeout: 0}}
  end

  @impl GenServer
  def handle_cast({:store, %{level: :debug} = log_entry}, state) do
    if state.high_load_timeout > 0 do
      {:noreply, state}
    else
      {:noreply, add_log_entry(log_entry, state)}
    end
  end

  def handle_cast({:store, log_entry}, state) do
    {:noreply, add_log_entry(log_entry, state)}
  end

  @impl GenServer
  def handle_call(:flush_once, _from, state) do
    {:reply, :ok, flush_logs(state)}
  end

  @impl GenServer
  def handle_info(:flush, state) do
    schedule_flush()
    {:noreply, flush_logs(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp filter_by_timestamp(scope, %{timestamp: since}), do: where(scope, [log], log.timestamp > ^since)
  defp filter_by_timestamp(scope, _), do: scope

  defp filter_by_id(scope, %{id: since}), do: where(scope, [log], log.id > ^since)
  defp filter_by_id(scope, _), do: scope

  defp filter_by_level(scope, %{level: :info}), do: where(scope, [log], log.level != :debug)
  defp filter_by_level(scope, %{level: :warn}), do: where(scope, [log], log.level == :warn or log.level == :error)
  defp filter_by_level(scope, %{level: :error}), do: where(scope, [log], log.level == :error)
  defp filter_by_level(scope, _), do: scope

  defp filter_by_source(scope, %{source: :air}), do: where(scope, [log], log.source == :air)
  defp filter_by_source(scope, %{source: :cloak}), do: where(scope, [log], log.source == :cloak)
  defp filter_by_source(scope, _), do: scope

  defp add_log_entry(log_entry, state) do
    logs = [log_entry | state.logs]
    logs_count = state.logs_count + 1

    high_load_timeout =
      if logs_count > @high_load_threshold do
        if state.high_load_timeout == 0 do
          Task.start(fn ->
            Logger.warn("Debug messages are temporarily discarded due to high volume of logs.")
          end)
        end

        @high_load_timeout_max
      else
        state.high_load_timeout
      end

    %{
      logs: logs,
      logs_count: logs_count,
      high_load_timeout: high_load_timeout
    }
  end

  defp schedule_flush(), do: Process.send_after(self(), :flush, @flush_interval)

  defp flush_logs(%{logs: logs, high_load_timeout: high_load_timeout}) do
    insert_logs(logs)
    %{logs: [], logs_count: 0, high_load_timeout: max(high_load_timeout - 1, 0)}
  end

  defp insert_logs([]), do: :ok
  defp insert_logs(logs), do: Repo.insert_all(Log, Enum.reverse(logs))

  if Mix.env() == :test do
    defp start_log_collection(), do: :ok
  else
    defp start_log_collection(), do: Logger.add_backend(Air.Service.Logs.Collector, flush: true)
  end
end
