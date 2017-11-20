defmodule Aircloak.OxfordComma do
  @moduledoc "Utility for joining parts with an Oxford comma."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Joins a parts with a comma"
  @spec join([String.t]) :: String.t
  def join(parts), do:
    join(parts, 0)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp join([part], _), do: part
  defp join([first, second], 0), do: first <> " and " <> second
  defp join([first, second], _), do: first <> ", and " <> second
  defp join([part | rest], n), do: part <> ", " <> join(rest, n + 1)
end
