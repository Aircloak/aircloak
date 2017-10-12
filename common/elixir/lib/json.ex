defmodule Aircloak.Json do
  @moduledoc "Utility for working with JSON files."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Decodes a JSON file, capturing a wide wariety of potential errors."
  @spec safe_decode(String.t) :: {:ok, Map.t} | {:error, String.t}
  def safe_decode(raw_json) do
    try do
      {:ok, Poison.decode!(raw_json)}
    rescue
      e -> {:error, e.message}
    end
  end
end
