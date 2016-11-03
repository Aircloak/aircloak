defmodule Central.StatsView do
  @moduledoc false
  use Central.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  defp to_int(val), do: Decimal.round(val)
end
