defmodule Air.Service.Password.Internal do
  @moduledoc "Service module for tasks surrounding working with passwords "

  @type raw_credentials :: %{login: String.t(), password: String.t()}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses user accounts and passwords from file content"
  @spec parse_credentials(String.t()) :: [raw_credentials()]
  def parse_credentials(contents) do
    contents
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Enum.filter(&is_map/1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parse_line(line) do
    with [login, password] <- String.split(line, ":"), do: %{login: login, password: password}
  end
end
