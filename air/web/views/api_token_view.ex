defmodule Air.ApiTokenView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  defp access(%{access: :api}), do: "API"
  defp access(%{access: :monitoring}), do: "Monitoring"
  defp access(%{access: access}), do: access
end
