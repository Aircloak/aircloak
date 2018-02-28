defmodule AirWeb.Admin.AuditLogView do
  @moduledoc false
  use Air.Web, :view

  import Scrivener.HTML

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

  defp audit_logs_to_json(audit_logs) do
    audit_logs.entries
    |> Enum.map(fn(audit_log) ->
      %{
        event: audit_log.event,
        user: audit_log.user,
        time: audit_log.inserted_at,
        metadata: audit_log.metadata,
      }
    end)
    |> to_json()
  end

  defp from(%Plug.Conn{query_params: query_params}), do:
    Map.get(query_params, "from", Timex.now() |> Timex.shift(days: -1) |> Timex.format!("{ISOdate} {ISOtime}"))

  defp to(%Plug.Conn{query_params: query_params}), do:
    Map.get(query_params, "to", Timex.now() |> Timex.format!("{ISOdate} {ISOtime}"))

  defp serialize_params(%Plug.Conn{query_params: query_params}) do
    Enum.flat_map(query_params, fn
      {name, values} when is_list(values) -> Enum.map(values, &{name <> "[]", &1})
      other -> [other]
    end)
  end
end
