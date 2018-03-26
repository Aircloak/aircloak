defmodule Cloak.DataSource.Parameters do
  @moduledoc """
  Module for working with database parameters, accounting for varied spellings
  """

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns a value for a key from a map, allowing for matches where the key in the map
  and the key used to lookup the value are of different types and written with distinct cases.

  For example:

    assert "value" === get(%{:key => "value"}, "KEY")
  """
  @spec get(map, String.t() | atom) :: String.t()
  def get(parameters, name) do
    [value] = all(parameters, name)
    value
  end

  @doc """
  Looks up a value under one of many possible keys. Returns the first value that is found
  or nil if none is found. Like `get/2`, the case of the key is not important.
  """
  @spec get_one_of(map, [String.t() | atom]) :: String.t() | nil
  def get_one_of(parameters, names) do
    Enum.find_value(names, &find_parameter_by_name(parameters, &1))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp find_parameter_by_name(parameters, name) do
    case all(parameters, name) do
      [] -> nil
      [value | _values] -> value
    end
  end

  defp all(parameters, name) do
    name = normalize_key(name)

    parameters
    |> Enum.map(fn {key, value} -> {normalize_key(key), value} end)
    |> Enum.filter(fn {key, _value} -> key === name end)
    |> Enum.map(fn {_key, value} -> value end)
  end

  defp normalize_key(key) when is_atom(key), do: normalize_key(Atom.to_string(key))

  defp normalize_key(key) do
    key
    |> String.trim()
    |> String.downcase()
  end
end
