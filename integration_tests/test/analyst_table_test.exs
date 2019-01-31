defmodule IntegrationTest.AnalystTableTest do
  use ExUnit.Case, async: false

  alias IntegrationTest.Manager
  import Aircloak.AssertionHelper

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  test "successful table creation", context do
    name = unique_name()
    assert {:ok, table} = create_table(context.user, name, "select user_id, name from users")
    assert table.name == name
    assert table.sql == "select user_id, name from users"
  end

  test "selecting from an analyst table", context do
    name = unique_name()
    {:ok, _table} = create_table(context.user, name, "select user_id, name from users")
    assert {:ok, result} = run_query(context.user, "select * from #{name}")
    assert result.columns == ~w(user_id name)
    assert result.buckets == [%{"occurrences" => 100, "row" => ["*", "john"], "unreliable" => false}]
  end

  test "cloak error is reported", context do
    name = unique_name()
    assert {:error, changeset} = create_table(context.user, name, "select foo")
    assert changeset.errors[:sql] == {"Expected `from` at line 1, column 11.", []}
  end

  test "duplicate name error is reported", context do
    name = unique_name()
    {:ok, _table} = create_table(context.user, name, "select user_id, name from users")
    assert {:error, changeset} = create_table(context.user, name, "select user_id, name from users")
    assert changeset.errors[:name] == {"has already been taken", []}
  end

  test "successful table update", context do
    name = unique_name()
    new_name = unique_name()

    {:ok, table} = create_table(context.user, name, "select user_id, name from users")
    assert {:ok, updated_table} = update_table(table.id, new_name, "select user_id from users")

    assert updated_table.id == table.id
    assert updated_table.name == new_name
    assert updated_table.sql == "select user_id from users"
    refute table.registration_info == updated_table.registration_info

    assert {:ok, result} = run_query(context.user, "select * from #{new_name}")
    assert result.columns == ~w(user_id)
    assert result.buckets == [%{"occurrences" => 100, "row" => ["*"], "unreliable" => false}]
  end

  test "after the cloak reconnects, it will receive analyst tables from the air", context do
    Air.Repo.delete_all(Air.Schemas.AnalystTable)
    table_name = unique_name()
    {:ok, _} = create_table(context.user, table_name, "select user_id from users")
    Manager.restart_cloak()

    assert soon(
             not Enum.empty?(Cloak.AnalystTable.analyst_tables(context.user.id, Manager.data_source())),
             :timer.seconds(5)
           )

    assert [%{name: ^table_name}] = Cloak.AnalystTable.analyst_tables(context.user.id, Manager.data_source())
    assert {:ok, result} = run_query(context.user, "select * from #{table_name}")
    assert result.columns == ~w(user_id)
    assert result.buckets == [%{"occurrences" => 100, "row" => ["*"], "unreliable" => false}]
  end

  defp unique_name(), do: "table_#{:erlang.unique_integer([:positive])}"

  defp create_table(user, name, sql), do: Air.Service.AnalystTable.create(user, Manager.data_source(), name, sql)

  defp update_table(table_id, name, sql), do: Air.Service.AnalystTable.update(table_id, name, sql)

  defp run_query(user, query, params \\ []) do
    data_source_id_spec = {:id, Manager.data_source().id}
    {:ok, query} = Air.Service.Query.create(data_source_id_spec, :autogenerate, user, :http, query, params, [])
    Air.Service.DataSource.await_query(query)
  end
end
