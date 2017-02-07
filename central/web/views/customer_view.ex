defmodule Central.CustomerView do
  @moduledoc false
  use Central.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Central.Schemas.{Air, Cloak}

  defp air_and_cloak_rows(customer) do
    customer.airs
    |> Enum.map(&normalize_air/1)
    |> Enum.sort_by(&(&1.name))
    |> Enum.flat_map(fn(air) ->
      usage_info_data = air
      |> Central.Service.Customer.latest_usage_info()
      |> Map.get(:data, %{})
      air.cloaks
      |> Enum.sort_by(&(&1.name))
      |> Enum.with_index()
      |> Enum.map(fn({%Cloak{name: cloak_name} = cloak, cloak_index}) ->
        air_version = Map.get(usage_info_data, "air_version", "Unknown")
        cloak_verison = Map.get(usage_info_data, "cloaks", [])
        |> Enum.find_value("Unknown", fn(cloak_info) ->
          if cloak_info["name"] == cloak_name do
            cloak_info["version"]
          else
            nil
          end
        end)
        %{
          air: air,
          air_version: air_version,
          cloak: cloak,
          cloak_version: cloak_verison,
          first_cloak?: cloak_index == 0
        }
      end)
    end)
  end

  defp normalize_air(%Air{cloaks: []} = air), do:
    %Air{air | cloaks: [%{name: "", status: :offline, data_source_names: []}]}
  defp normalize_air(%Air{cloaks: [_|_]} = air), do:
    air
end
