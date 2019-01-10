defmodule AirWeb.Admin.AnalysisView do
  @moduledoc false
  use Air.Web, :view

  def total(groups, field), do: groups |> Enum.map(& &1[field]) |> Enum.sum()
end
