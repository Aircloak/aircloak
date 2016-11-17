defmodule IntegrationTest.QueryTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  test "show tables" do
    assert {:ok, result} = run_query("show tables")
    assert Map.fetch!(result, "columns") == ["name"]
    assert result |> Map.fetch!("features") |> Map.fetch!("column_types") == ["text"]
    assert result |> Map.fetch!("features") |> Map.fetch!("selected_types") == ["text"]
    assert Map.fetch!(result, "rows") == [%{"occurrences" => 1, "row" => ["users"]}]
  end

  test "show columns" do
    {:ok, result} = run_query("show columns from users")

    assert Map.fetch!(result, "rows") == [
      %{"occurrences" => 1, "row" => ["user_id", "text"]},
      %{"occurrences" => 1, "row" => ["name", "text"]},
      %{"occurrences" => 1, "row" => ["height", "integer"]}
    ]
  end

  test "select" do
    {:ok, result} = run_query("select name, height from users")
    assert Map.fetch!(result, "rows") == [%{"occurrences" => 100, "row" => ["john", 180]}]
  end

  defp run_query(query, params \\ []), do:
    Air.Service.DataSource.run_query(Manager.data_source_global_id(), Manager.air_user(), query, params)
end
