defmodule Cloak.DataSource.Parameters do
  @moduledoc """
  Module for working with database parameters, accounting for varied spellings
  """


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  @spec get(Map.t, String.t) :: String.t
  def get(parameters, name) do
    [value] = all(parameters, name)
    value
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp all(parameters, name) do
    name = normalize_key(name)
    parameters
    |> Enum.map(fn({key, value}) -> {normalize_key(key), value} end)
    |> Enum.filter_map(fn({key, _value}) -> key === name end, fn({_key, value}) -> value end)
  end

  defp normalize_key(key) when is_atom(key), do: normalize_key(Atom.to_string(key))
  defp normalize_key(key) do
    key
    |> String.trim()
    |> String.downcase()
  end
end
