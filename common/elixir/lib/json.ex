defmodule Aircloak.Json do
  @moduledoc "Utility for working with JSON files."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Decodes a JSON file, capturing a wide wariety of potential errors."
  @spec safe_decode(String.t) :: {:ok, Map.t} | {:error, String.t}
  def safe_decode(raw_json) do
    try do
      # Note the usage of decode! (which throws) rather than decode.
      # The latter doesn't return a usable error message.
      {:ok, Poison.decode!(raw_json)}
    rescue
      # Different version of Poison throw various exceptions, so we
      # are throwing a wide net, catching everything and everything.
      e -> {:error, e.message}
    end
  end
end
