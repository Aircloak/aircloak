defmodule Air.Service.Logs do
  @moduledoc "Service gathering logs."

  alias Air.Repo
  alias Air.Schemas.Log
  import Ecto.Query

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Stores a new log entry into the database."
  @spec save(String.t(), Log.MessageSource.t(), NaiveDateTime.t(), String.t()) :: :ok
  def save(hostname, source, timestamp, message) do
    %Log{hostname: hostname, source: source, timestamp: timestamp, message: message}
    |> Repo.insert()

    :ok
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

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp filter_by_timestamp(scope, %{timestamp: since}), do: where(scope, [log], log.timestamp > ^since)
  defp filter_by_timestamp(scope, _), do: scope

  defp filter_by_id(scope, %{id: since}), do: where(scope, [log], log.id > ^since)
  defp filter_by_id(scope, _), do: scope
end
