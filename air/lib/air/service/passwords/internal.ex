defmodule Air.Service.Password.Internal do
  @moduledoc "Service module for tasks surrounding working with passwords "

  @type raw_credentials :: %{login: String.t(), password: String.t(), admin: boolean}

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
    case String.split(line, ":") do
      [login, password, "admin"] -> %{login: login, password: password, admin: true}
      [login, password, _] -> %{login: login, password: password, admin: false}
      [login, password] -> %{login: login, password: password, admin: false}
      _line -> :error
    end
  end
end
