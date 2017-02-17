defmodule Air.Admin.CentralView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  defp allow_new_export?(nil), do: false
  defp allow_new_export?(_), do: true
end
