defmodule Aircloak.Json do
  @moduledoc "Utility for working with JSON files."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Decodes a JSON file, capturing a wide wariety of potential errors."
  @spec safe_decode(String.t()) :: {:ok, Map.t()} | {:error, String.t()}
  def safe_decode(raw_json) do
    # We're using decode! instead of decode since the latter doesn't return a usable error message
    {:ok, Poison.decode!(raw_json)}
  rescue
    # Different versions of Poison throw various exceptions, so we're catching everything
    e ->
      {:error, e.message}
  end
end
