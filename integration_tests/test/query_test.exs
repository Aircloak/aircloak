defmodule IntegrationTest.QueryTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  test "show tables" do
    assert {:ok, result} = run_query("show tables")
    assert ["name"] == Map.fetch!(result, "columns")
    assert ["text"] == result |> Map.fetch!("features") |> Map.fetch!("column_types")
    assert ["text"] == result |> Map.fetch!("features") |> Map.fetch!("selected_types")
  end

  defp run_query(query, params \\ []), do:
    Air.Service.DataSource.run_query(Manager.data_source_global_id(), Manager.air_user(), query, params)
end
