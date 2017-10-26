defmodule Air.Admin.CloaksView do
  @moduledoc false
  use Air.Web, :view

  def time_ago(date_time) do
    Timex.from_now(date_time)
  end

  defdelegate availability_label(data_source), to: Air.DataSourceView
end
