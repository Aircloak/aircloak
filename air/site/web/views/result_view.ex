defmodule Air.ResultView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  defp to_js_timestamp(date_time) do
    Ecto.DateTime.to_iso8601(date_time)
  end
end
