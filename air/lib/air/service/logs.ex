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

  @doc "Returns the most recent log entries, sorted in descendent order by timestamp."
  @spec tail(NaiveDateTime.t(), pos_integer()) :: [Log.t()]
  def tail(since, max_entries) do
    Log
    |> where([log], log.timestamp > ^since)
    |> order_by([log], desc: log.timestamp)
    |> limit(^max_entries)
    |> Repo.all()
  end
end
