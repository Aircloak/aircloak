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

  def is_selected(%Plug.Conn{query_params: query_params}, name, param), do:
    is_selected(query_params, name, param)
  def is_selected(query_params, name, param) do
    normalized_name = to_string(name)
    normalized_param = to_string(param)
    values_for_name = query_params[normalized_name] || []
    Enum.find(values_for_name, & &1 == normalized_param) != nil
  end

  def toggle_item(%Plug.Conn{query_params: query_params}, name, param), do:
    toggle_item(query_params, name, param)
  def toggle_item(query_params, name, param) do
    normalized_name = to_string(name)
    normalized_param = to_string(param)
    values_for_name = query_params[normalized_name] || []
    values_for_name = if is_selected(query_params, normalized_name, normalized_param) do
      values_for_name -- [normalized_param]
    else
      [normalized_param | values_for_name]
    end
    Map.put(query_params, normalized_name, values_for_name)
  end

  def glyph(true), do: "<span class='glyphicon glyphicon-remove' aria-hidden='true'></span> "
  def glyph(false), do: ""
end
