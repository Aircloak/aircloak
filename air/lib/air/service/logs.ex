defmodule Air.Service.Logs do
  @moduledoc "Service for logs management."

  alias Air.Repo
  alias Air.Schemas.Log
  import Ecto.Query

  use GenServer

  @flush_interval :timer.seconds(1)
  @high_load_threshold 100
  @high_load_timeout 5

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Queues a new log entry for saving into the database."
  @spec save(NaiveDateTime.t(), Log.Source.t(), String.t(), Log.Level.t(), String.t()) :: :ok
  def save(timestamp, source, hostname, level, message) do
    log_entry = %{timestamp: timestamp, source: source, hostname: hostname, level: level, message: message}
    GenServer.cast(__MODULE__, {:store, log_entry})
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

  @doc "Converts Logger timestamp to NaiveDateTime."
  def convert_logger_timestamp({date, time}) do
    {hour, minute, second, millisecond} = time
    NaiveDateTime.from_erl!({date, {hour, minute, second}}, {millisecond * 1000, 6})
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
    {:ok, %{logs: [], logs_count: 0, high_load_remaining_timeout: 0}}
  end

  @impl GenServer
  def handle_cast(
        {:store, %{level: :debug}},
        %{high_load_remaining_timeout: high_load_remaining_timeout} = state
      )
      when high_load_remaining_timeout > 0 do
    {:noreply, state}
  end

  def handle_cast({:store, log_entry}, state) do
    {:noreply, add_log_entry(log_entry, state)}
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

    cond do
      logs_count > @high_load_threshold and state.high_load_remaining_timeout == 0 ->
        %{
          logs: add_warning(logs),
          logs_count: logs_count + 1,
          high_load_remaining_timeout: @high_load_timeout
        }

      logs_count > @high_load_threshold ->
        %{
          logs: logs,
          logs_count: logs_count,
          high_load_remaining_timeout: @high_load_timeout
        }

      true ->
        %{
          logs: logs,
          logs_count: logs_count,
          high_load_remaining_timeout: state.high_load_remaining_timeout
        }
    end
  end

  defp schedule_flush(), do: Process.send_after(self(), :flush, @flush_interval)

  defp flush_logs(%{logs: logs, high_load_remaining_timeout: high_load_remaining_timeout}) do
    insert_logs(logs)
    %{logs: [], logs_count: 0, high_load_remaining_timeout: max(high_load_remaining_timeout - 1, 0)}
  end

  defp insert_logs([]), do: :ok
  defp insert_logs(logs), do: Repo.insert_all(Log, Enum.reverse(logs))

  defp add_warning(logs) do
    {:ok, hostname} = :inet.gethostname()

    [
      %{
        timestamp: Logger.Utils.timestamp(false) |> convert_logger_timestamp(),
        source: :air,
        hostname: to_string(hostname),
        level: :warn,
        message: "Debug messages are temporarily discarded due to high volume of logs."
      }
      | logs
    ]
  end

  if Mix.env() == :test do
    defp start_log_collection(), do: :ok
  else
    defp start_log_collection(), do: Logger.add_backend(Air.Service.Logs.Collector, flush: true)
  end
end
