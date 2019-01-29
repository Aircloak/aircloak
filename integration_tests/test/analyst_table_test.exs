defmodule IntegrationTest.AnalystTableTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  test "error is reported", context do
    assert {:error, "Expected `from` at line 1, column 11."} = store_table(context.user, "foo", "select foo")
  end

  test "successful table creation", context do
    assert :ok = store_table(context.user, "foo", "select user_id, name from users")
  end

  test "selecting from a table", context do
    :ok = store_table(context.user, "foo", "select user_id, name from users")
    assert {:ok, result} = run_query(context.user, "select * from foo")
    assert result.columns == ~w(user_id name)
    assert result.buckets == [%{"occurrences" => 100, "row" => ["*", "john"], "unreliable" => false}]
  end

  defp store_table(user, name, sql), do: Air.Service.AnalystTable.create(user, Manager.data_source(), name, sql)

  defp run_query(user, query, params \\ []) do
    data_source_id_spec = {:id, Manager.data_source().id}
    {:ok, query} = Air.Service.Query.create(data_source_id_spec, :autogenerate, user, :http, query, params, [])
    Air.Service.DataSource.await_query(query)
  end
end
