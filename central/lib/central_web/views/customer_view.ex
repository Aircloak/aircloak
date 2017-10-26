defmodule CentralWeb.CustomerView do
  @moduledoc false
  use Central.Web, :view
  alias Central.Schemas.Air

  defp air_and_cloak_rows(customer) do
    for air <- customer.airs |> Enum.map(&normalize_air/1) |> Enum.sort_by(&(&1.name)),
        {cloak, cloak_index} <- air.cloaks |> Enum.sort_by(&(&1.name)) |> Enum.with_index() do
      %{air: air, cloak: cloak, first_cloak?: cloak_index == 0}
    end
  end

  defp normalize_air(%Air{cloaks: []} = air), do:
    %Air{air | cloaks: [%{name: "", status: :offline, data_source_names: [], version: "Unknown"}]}
  defp normalize_air(%Air{cloaks: [_|_]} = air), do:
    air
end
