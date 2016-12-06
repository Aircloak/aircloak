defmodule IntegrationTest.QueryTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  test "show tables", context do
    assert {:ok, result} = run_query(context.user, "show tables")
    assert Map.fetch!(result, "columns") == ["name"]
    assert result |> Map.fetch!("features") |> Map.fetch!("column_types") == ["text"]
    assert result |> Map.fetch!("features") |> Map.fetch!("selected_types") == ["text"]
    assert Map.fetch!(result, "rows") == [%{"occurrences" => 1, "row" => ["users"]}]
  end

  test "show columns", context do
    {:ok, result} = run_query(context.user, "show columns from users")

    assert Map.fetch!(result, "rows") == [
      %{"occurrences" => 1, "row" => ["user_id", "text"]},
      %{"occurrences" => 1, "row" => ["name", "text"]},
      %{"occurrences" => 1, "row" => ["height", "integer"]}
    ]
  end

  test "select", context do
    {:ok, result} = run_query(context.user, "select name, height from users")
    assert Map.fetch!(result, "rows") == [%{"occurrences" => 100, "row" => ["john", 180]}]
  end

  defp run_query(user, query, params \\ []), do:
    Air.Service.DataSource.run_query(
      {:global_id, Manager.data_source_global_id()},
      user,
      query,
      params
    )
end
