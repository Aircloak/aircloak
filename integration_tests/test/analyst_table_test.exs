defmodule IntegrationTest.AnalystTableTest do
  use ExUnit.Case, async: false

  alias IntegrationTest.Manager
  import Aircloak.AssertionHelper

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  test "supports flag is passed to air and stored in database",
    do: assert(Manager.data_source().supports_analyst_tables == true)

  test "successful table creation", context do
    name = unique_name()
    assert {:ok, table} = create_table(context.user, name, "select user_id, name from users")
    assert table.name == name
    assert table.sql == "select user_id, name from users"
    assert [column1, column2] = Enum.sort_by(table.result_info.columns, & &1.name)
    assert %{name: "name", type: "text", user_id: false} = column1
    assert %{name: "user_id", type: "text", user_id: true} = column2
    refute is_nil(Air.Repo.get(Air.Schemas.AnalystTable, table.id))
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

  test "create changeset error takes same shape irrespective of origin", context do
    # Immediate failure due to missing name before the query is validated by the Cloak
    {:error,
     immediate_changeset = %Ecto.Changeset{
       action: immediate_action,
       changes: immediate_changes
     }} = create_table(context.user, nil, "Invalid SQL")

    # Rejected by the Cloak due to invalid SQL
    {:error,
     cloak_changeset = %Ecto.Changeset{
       action: cloak_action,
       changes: cloak_changes
     }} = create_table(context.user, "tableName", "Invalid SQL")

    refute immediate_changeset.valid?
    refute cloak_changeset.valid?
    assert immediate_action == :insert
    assert immediate_action == cloak_action
    assert immediate_changes == Map.drop(cloak_changes, [:name])
  end

  test "update changeset error takes same shape irrespective of origin", context do
    {:ok, table} = create_table(context.user, "tableName", "select user_id, name from users")

    # Immediate failure due to missing name before the query is validated by the Cloak
    {:error,
     immediate_changeset = %Ecto.Changeset{
       action: immediate_action,
       changes: immediate_changes
     }} = update_table(table.id, context.user, nil, "Invalid SQL")

    # Rejected by the Cloak due to invalid SQL
    {:error,
     cloak_changeset = %Ecto.Changeset{
       action: cloak_action,
       changes: cloak_changes
     }} = update_table(table.id, context.user, "tableName", "Invalid SQL")

    refute immediate_changeset.valid?
    refute cloak_changeset.valid?
    assert immediate_action == :update
    assert immediate_action == cloak_action
    assert immediate_changes == Map.drop(cloak_changes, [:name])
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
    assert {:ok, updated_table} = update_table(table.id, context.user, new_name, "select user_id from users")

    assert updated_table.id == table.id
    assert updated_table.name == new_name
    assert updated_table.sql == "select user_id from users"
    refute table.result_info.registration_info == updated_table.result_info.registration_info
    assert [%{name: "user_id", type: "text", user_id: true}] = updated_table.result_info.columns

    assert {:ok, result} = run_query(context.user, "select * from #{new_name}")
    assert result.columns == ~w(user_id)
    assert result.buckets == [%{"occurrences" => 100, "row" => ["*"], "unreliable" => false}]
  end

  test "failed update when other user", context do
    name = unique_name()
    new_name = unique_name()
    other_user = Manager.create_air_user()

    {:ok, table} = create_table(context.user, name, "select user_id, name from users")
    assert {:error, :not_allowed} = update_table(table.id, other_user, new_name, "select user_id from users")
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

  test "successful table delete", context do
    name = unique_name()

    {:ok, table} = create_table(context.user, name, "select user_id, name from users")
    db_name = table.result_info.registration_info |> Jason.decode!() |> Map.fetch!("db_name")

    assert :ok = Air.Service.AnalystTable.delete(table.id, context.user)
    assert table_not_in_db?(db_name)
    assert is_nil(Air.Repo.get(Air.Schemas.AnalystTable, table.id))
  end

  test "analyst table can reference a view", context do
    name = unique_name()

    {:ok, _} =
      Air.Service.View.create(context.user, Manager.data_source(), "some_view", "select user_id, name from users")

    {:ok, _table} = create_table(context.user, name, "select * from some_view")

    assert {:ok, result} = run_query(context.user, "select * from #{name}")
    assert result.columns == ~w(user_id name)
    assert result.buckets == [%{"occurrences" => 100, "row" => ["*", "john"], "unreliable" => false}]
  end

  test "analyst table name can't be the same as a name of an existing view", context do
    name = unique_name()

    {:ok, _} = Air.Service.View.create(context.user, Manager.data_source(), name, "select user_id, name from users")

    assert {:error, changeset} = create_table(context.user, name, "select * from some_view")
    assert changeset.errors[:name] == {"has already been taken", []}
  end

  defp unique_name(), do: "table_#{:erlang.unique_integer([:positive])}"

  defp create_table(user, name, sql) do
    with {:ok, table} <- Air.Service.AnalystTable.create(user, Manager.data_source(), name, sql) do
      assert soon(table_created?(user.id, name, Manager.data_source()), :timer.seconds(5), repeat_wait_time: 10)
      {:ok, table}
    end
  end

  defp update_table(table_id, user, name, sql) do
    with {:ok, table} <- Air.Service.AnalystTable.update(table_id, user, name, sql) do
      assert soon(table_created?(table.user_id, name, Manager.data_source()), :timer.seconds(5), repeat_wait_time: 10)
      {:ok, table}
    end
  end

  defp table_created?(analyst_id, name, data_source) do
    {:ok, cloak_data_source} = Cloak.DataSource.fetch(data_source.name)

    with table <- Cloak.AnalystTable.find(analyst_id, name, cloak_data_source),
         false <- is_nil(table),
         true <- table.status != :creating,
         do: true,
         else: (_ -> false)
  end

  defp table_not_in_db?(db_name) do
    {:ok, data_source} = Cloak.DataSource.fetch(Manager.data_source().name)

    data_source
    |> Cloak.DataSource.Connection.execute!(&data_source.driver.analyst_tables/1)
    |> Enum.all?(&(&1 != db_name))
  end

  defp run_query(user, query, params \\ []) do
    data_source_id_spec = {:id, Manager.data_source().id}
    {:ok, query} = Air.Service.Query.create(data_source_id_spec, :autogenerate, user, :http, query, params, [])
    Air.Service.DataSource.await_query(query)
  end
end
