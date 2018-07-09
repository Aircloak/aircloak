defmodule AirWeb.Admin.QueryView do
  @moduledoc false
  use Air.Web, :view

  defp time_ago(entry), do: Air.Utils.DateTime.time_ago(entry.inserted_at)

  defp absolute_time(entry), do: Timex.format!(entry.inserted_at, "{ISOdate} {h24}:{m}:{s}")

  defp data_source_name(%{data_source: nil}), do: ""
  defp data_source_name(%{data_source: data_source}), do: data_source.name
end
