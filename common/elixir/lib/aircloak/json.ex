defmodule Aircloak.Json do
  @moduledoc "Utility for working with JSON files."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Decodes a string with data encoded as JSON into a map, capturing a wide wariety of potential errors
  and producing a descriptive error message.

  This implementation uses Poison for decoding rather than Jason. Jason is more restrictive in its
  adherence to the JSON spec, and therefore does not allow multiline strings which are required to
  allow queries to be defined inline in data source definitions:

    "accounts": {
      "query": "
        SELECT uid, name, balance
        FROM transactions
        ORDER BY date
        LIMIT 1
      ",
      "user_id": "uid"
    }

  And furthermore Jason outputs error messages which are impossible to parse.
  """
  @spec permissive_decode(String.t()) :: {:ok, Map.t()} | {:error, String.t()}
  def permissive_decode(raw_json) do
    # We're using decode! instead of decode since the latter doesn't return a usable error message
    {:ok, Poison.decode!(raw_json)}
  rescue
    # Different versions of Poison throw various exceptions, so we're catching everything
    e ->
      {:error, e.message}
  end
end
