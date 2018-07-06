defmodule AirWeb.Admin.SharedView do
  @moduledoc false
  use Air.Web, :view

  defp serialize_params(%Plug.Conn{query_params: query_params}) do
    Enum.flat_map(query_params, fn
      {name, values} when is_list(values) -> Enum.map(values, &{name <> "[]", &1})
      other -> [other]
    end)
  end
end
