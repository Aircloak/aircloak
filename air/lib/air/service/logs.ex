defmodule Air.Service.Logs do
  @moduledoc "Service gathering logs."

  alias Air.Repo
  alias Air.Schemas.Log

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
end
