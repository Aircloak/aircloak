defmodule Air.Service.Logs do
  @moduledoc "Service for logs management."

  alias Air.Repo
  alias Air.Schemas.Log
  import Ecto.Query

  use GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores a new log entry into the database."
  @spec save(NaiveDateTime.t(), Log.Source.t(), String.t(), Log.Level.t(), String.t()) :: :ok
  def save(timestamp, source, hostname, level, message) do
    log_entry = %Log{timestamp: timestamp, source: source, hostname: hostname, level: level, message: message}
    GenServer.cast(__MODULE__, {:store, log_entry})
  end

  @doc "Returns the most recent log entries, sorted in ascendent order by timestamp."
  @spec tail(Map.t(), pos_integer()) :: [Log.t()]
  def tail(filters, max_entries) do
    Log
    |> filter_by_id(filters)
    |> filter_by_timestamp(filters)
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
    {:ok, nil}
  end

  @impl GenServer
  def handle_cast({:store, log_entry}, state) do
    Repo.insert!(log_entry)
    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp filter_by_timestamp(scope, %{timestamp: since}), do: where(scope, [log], log.timestamp > ^since)
  defp filter_by_timestamp(scope, _), do: scope

  defp filter_by_id(scope, %{id: since}), do: where(scope, [log], log.id > ^since)
  defp filter_by_id(scope, _), do: scope

  if Mix.env() == :test do
    defp start_log_collection(), do: :ok
  else
    defp start_log_collection(), do: Logger.add_backend(Air.Service.Logs.Collector, flush: true)
  end
end
