defmodule IntegrationTest.QueryTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup do
    {:ok, user: Manager.create_air_user()}
  end

  test "show tables", context do
    assert {:ok, result} = run_query(context.user, "show tables")
    assert result.columns == ["name"]
    assert result.features.column_types == ["text"]
    assert result.features.selected_types == ["text"]
    assert result.buckets == [%{"occurrences" => 1, "row" => ["users"]}]
  end

  test "show columns", context do
    {:ok, result} = run_query(context.user, "show columns from users")

    assert result.buckets == [
             %{"occurrences" => 1, "row" => ["user_id", "text"]},
             %{"occurrences" => 1, "row" => ["name", "text"]},
             %{"occurrences" => 1, "row" => ["height", "integer"]}
           ]
  end

  test "select", context do
    {:ok, result} = run_query(context.user, "select name, height from users")

    assert result.buckets == [
             %{"occurrences" => 100, "row" => ["john", 180], "unreliable" => false}
           ]
  end

  test "Query logs returned are truncated to second", context do
    {:ok, result} = run_query(context.user, "select name, height from users")

    log_lines =
      result.log
      |> String.split("\n")
      |> Enum.reject(&(&1 == ""))

    assert length(log_lines) > 0

    assert log_lines
           |> Enum.map(&Regex.match?(~r/^\d{1,2}:\d{1,2}:\d{1,2}.000/, &1))
           |> Enum.all?()
  end

  test "retrieval of query results as csv", context do
    {:ok, %{query_id: query_id}} = run_query(context.user, "select name, height from users")

    # by the time we get the result, the query might not be stored, so we'll sleep a bit
    :timer.sleep(200)

    assert [["name", "height"], ["john", "180"]] ==
             air_api_get!(context.user, "queries/#{query_id}.csv")
             |> Map.fetch!(:body)
             |> String.split("\r\n")
             |> Enum.reject(&(&1 == ""))
             |> CSV.decode()
             |> Enum.uniq()
  end

  test "query context is properly set", context do
    {:ok, %{query_id: query_id}} = run_query(context.user, "select name, height from users")
    {:ok, query} = Air.Service.Query.get_as_user(context.user, query_id)
    assert query.context == :http
  end

  defp air_api_get!(user, path),
    do:
      HTTPoison.get!("http://localhost:#{air_http_port()}/api/#{path}", %{
        "auth-token" => Air.Token.create_api_token(user, :api, "test token")
      })

  defp air_http_port(),
    do:
      Application.fetch_env!(:air, AirWeb.Endpoint)
      |> Keyword.fetch!(:http)
      |> Keyword.fetch!(:port)

  defp run_query(user, query, params \\ []) do
    {:ok, query} = Air.Service.Query.create(:autogenerate, user, :http, query, params, [])
    Air.Service.DataSource.run_query(query, {:name, Manager.data_source_name()})
  end
end
