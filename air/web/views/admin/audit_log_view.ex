defmodule Air.Admin.AuditLogView do
  @moduledoc false
  use Air.Web, :view

  import Scrivener.HTML

  defp humanize_names(metadata) do
    Enum.map(metadata, fn({name, value}) ->
      {Phoenix.Naming.humanize(name), value}
    end)
  end

  defp time_ago(entry), do: Air.Utils.DateTime.time_ago(entry.inserted_at)

  defp absolute_time(entry), do: Timex.format!(entry.inserted_at, "{ISOdate} {h24}:{m}:{s}")

  defp selected?(%Plug.Conn{query_params: query_params}, name, param), do:
    selected?(query_params, name, param)
  defp selected?(query_params, name, param) do
    Enum.any?(query_params[to_string(name)] || [], &(&1 == to_string(param)))
  end

  defp toggle_item(%Plug.Conn{query_params: query_params}, name, param) do
    normalized_name = to_string(name)
    normalized_param = to_string(param)
    values_for_name = query_params[normalized_name] || []
    values_for_name = if selected?(query_params, normalized_name, normalized_param) do
      values_for_name -- [normalized_param]
    else
      [normalized_param | values_for_name]
    end
    Map.put(query_params, normalized_name, values_for_name)
  end

  defp glyph(true), do: {:safe, "<span class='glyphicon glyphicon-remove' aria-hidden='true'></span> "}
  defp glyph(false), do: {:safe, ""}
end
