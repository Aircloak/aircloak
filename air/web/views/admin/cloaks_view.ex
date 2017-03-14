defmodule Air.Admin.CloaksView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  def time_ago(date_time) do
    Timex.from_now(date_time)
  end

  defdelegate availability_label(data_source), to: Air.DataSourceView
  
  def data_source_names(cloak_info) do
    cloak_info.data_sources
    |> Enum.map(&(&1.name))
    |> Enum.join(", ")
  end
end
