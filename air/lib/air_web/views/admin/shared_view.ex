defmodule AirWeb.Admin.SharedView do
  @moduledoc false
  use Air.Web, :view

  defp serialize_params(%Plug.Conn{query_params: query_params}) do
    Enum.flat_map(query_params, fn
      {name, values} when is_list(values) -> Enum.map(values, &{name <> "[]", &1})
      other -> [other]
    end)
  end

  defp selected?(%Plug.Conn{query_params: query_params}, name, param), do: selected?(query_params, name, param)

  defp selected?(query_params, name, param) do
    Enum.any?(query_params[to_string(name)] || [], &(&1 == to_string(param)))
  end

  defp toggle_item(query_params, name, param) do
    normalized_name = to_string(name)
    normalized_param = to_string(param)
    values_for_name = query_params[normalized_name] || []

    values_for_name =
      if selected?(query_params, normalized_name, normalized_param) do
        values_for_name -- [normalized_param]
      else
        [normalized_param | values_for_name]
      end

    Map.put(query_params, normalized_name, values_for_name)
  end

  defp filter_title(:users), do: "Filter by users"
  defp filter_title(:event_types), do: "Filter by event type"
  defp filter_title(:data_sources), do: "Filter by data source"
  defp filter_title(:query_states), do: "Filter by state"
end
