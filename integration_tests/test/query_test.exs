defmodule IntegrationTest.QueryTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager
  import IntegrationTest.Helpers

  setup do
    {:ok, user: Manager.create_admin_user()}
  end

  test "show tables", context do
    assert {:ok, result} = run_query(context.user, "show tables")
    assert result.columns == ["name", "type", "comment"]
    assert result.selected_types == ["text", "text", "text"]

    assert result.buckets == [
             %{"occurrences" => 1, "row" => ["column_access", "personal", ""]},
             %{"occurrences" => 1, "row" => ["integers", "personal", ""]},
             %{"occurrences" => 1, "row" => ["users", "personal", ""]}
           ]
  end

  test "show columns", context do
    {:ok, result} = run_query(context.user, "show columns from users")

    assert [
             %{"occurrences" => 1, "row" => ["user_id", "text", isolator1, "user_id", ""]},
             %{"occurrences" => 1, "row" => ["name", "text", isolator2, nil, ""]},
             %{"occurrences" => 1, "row" => ["height", "integer", isolator3, nil, ""]}
           ] = result.buckets

    assert Enum.all?(
             [isolator1, isolator2, isolator3],
             &(&1 in ["true", "false", "pending", "failed"])
           )
  end

  test "show columns ignores excluded columns", context do
    {:ok, result} = run_query(context.user, "show columns from column_access")

    assert [
             %{"occurrences" => 1, "row" => ["user_id", "text", _, "user_id", ""]},
             %{"occurrences" => 1, "row" => ["white", "integer", _, nil, ""]},
             %{"occurrences" => 1, "row" => ["grey", "integer", _, nil, ""]}
           ] = result.buckets
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
        "auth-token" => Air.Service.Token.create_api_token(user, :api, "test token")
      })

  defp air_http_port(),
    do:
      Application.fetch_env!(:air, AirWeb.Endpoint)
      |> Keyword.fetch!(:http)
      |> Keyword.fetch!(:port)
end
