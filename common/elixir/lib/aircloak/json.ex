defmodule Aircloak.Json do
  @moduledoc "Utility for working with JSON files."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Decodes a JSON string, converting an error into a string message."
  @spec safe_decode(String.t()) :: {:ok, Map.t()} | {:error, String.t()}
  def safe_decode(raw_json) do
    with {:error, error} <- Jason.decode(raw_json), do: {:error, Exception.message(error)}
  end
end
