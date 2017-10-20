defmodule Air.ApiTokenView do
  @moduledoc false
  use Air.Web, :view

  defp access(%{access: :api}), do: "API"
  defp access(%{access: :monitoring}), do: "Monitoring"
  defp access(%{access: access}), do: access
end
