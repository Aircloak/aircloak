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
    assert [%{"occurrences" => 100, "row" => ["john", 180]}] = Map.fetch!(result, "rows")
  end

  test "retrieval of query results as csv", context do
    {:ok, %{"query_id" => query_id}} = run_query(context.user, "select name, height from users")

    assert [["name", "height"], ["john", "180"]] ==
      air_api_get!(context.user, "queries/#{query_id}.csv")
      |> Map.fetch!(:body)
      |> String.split("\r\n")
      |> Enum.reject(&(&1 == ""))
      |> CSV.decode()
      |> Enum.uniq()
  end

  test "query context is properly set", context do
    {:ok, %{"query_id" => query_id}} = run_query(context.user, "select name, height from users")
    {:ok, query} = Air.Service.Query.get_as_user(context.user, query_id)
    assert query.context == :http
  end

  defp air_api_get!(user, path), do:
    HTTPoison.get!(
      "http://localhost:#{air_http_port()}/api/#{path}",
      %{"auth-token" => Air.Token.create_api_token(user, "test token")}
    )

  defp air_http_port(), do:
    Application.fetch_env!(:air, Air.Endpoint)
    |> Keyword.fetch!(:http)
    |> Keyword.fetch!(:port)

  defp run_query(user, query, params \\ []), do:
    Air.Service.DataSource.run_query(
      {:name, Manager.data_source_name()},
      user,
      :http,
      query,
      params
    )
end
