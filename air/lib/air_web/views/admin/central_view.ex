defmodule AirWeb.Admin.CentralView do
  @moduledoc false
  use Air.Web, :view

  import Scrivener.HTML

  defp allow_new_export?(nil), do: false
  defp allow_new_export?(_), do: true
end
