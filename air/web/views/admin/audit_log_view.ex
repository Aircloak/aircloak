defmodule Air.Admin.AuditLogView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  import Scrivener.HTML

  def humanize_names(metadata) do
    Enum.map(metadata, fn({name, value}) ->
      {Phoenix.Naming.humanize(name), value}
    end)
  end

  def time_ago(entry), do: Air.Utils.DateTime.time_ago(entry.inserted_at)

  def absolute_time(entry), do: Timex.format!(entry.inserted_at, "{ISOdate} {h24}:{m}:{s}")

  def selected?(%Plug.Conn{query_params: query_params}, name, param), do:
    selected?(query_params, name, param)
  def selected?(query_params, name, param) do
    Enum.any?(query_params[to_string(name)] || [], &(&1 == to_string(param)))
  end

  def toggle_item(%Plug.Conn{query_params: query_params}, name, param) do
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

  def glyph(true), do: {:safe, "<span class='glyphicon glyphicon-remove' aria-hidden='true'></span> "}
  def glyph(false), do: {:safe, ""}
end
