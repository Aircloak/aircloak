defmodule Air.Admin.QueryView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  import Scrivener.HTML

  defp time_ago(entry), do: Air.Utils.DateTime.time_ago(entry.inserted_at)

  defp absolute_time(entry), do: Timex.format!(entry.inserted_at, "{ISOdate} {h24}:{m}:{s}")
end
