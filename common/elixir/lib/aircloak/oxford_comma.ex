defmodule Aircloak.OxfordComma do
  @moduledoc "Utility for joining parts with an Oxford comma."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Joins parts with a comma adding an `and` before the last part"
  @spec join([String.t()], String.t()) :: String.t()
  def join(parts, connection \\ "and")
  def join([], _), do: ""
  def join(parts, conjunction), do: join(parts, 0, conjunction)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp join([part], _, _), do: part
  defp join([first, second], 0, conjunction), do: first <> " #{conjunction} " <> second
  defp join([first, second], _, conjunction), do: first <> ", #{conjunction} " <> second
  defp join([part | rest], n, conjunction), do: part <> ", " <> join(rest, n + 1, conjunction)
end
