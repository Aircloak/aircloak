defmodule IntegrationTest.AnalystTableTest do
  use ExUnit.Case, async: false

  alias IntegrationTest.Manager
  import Aircloak.AssertionHelper

  setup_all do
    Air.Repo.delete_all(Air.Schemas.AnalystTable)

    {:ok, cloak_data_source} = Cloak.DataSource.fetch(Manager.data_source().name)

    Cloak.DataSource.Connection.execute!(
      cloak_data_source,
      &cloak_data_source.driver.execute!(&1, "truncate table __ac_analyst_tables_1")
    )

    {:ok, user: Manager.create_air_user()}
  end

  test "supports flag is passed to air and stored in database",
    do: assert(Manager.data_source().supports_analyst_tables == true)

  test "successful table creation", context do
    name = unique_name()
    assert {:ok, table} = create_table(context.user, name, "select user_id, name from users")
    assert table.name == name
    assert table.sql == "select user_id, name from users"
    assert [column1, column2] = Enum.sort_by(table.columns, & &1.name)
    assert %{name: "name", type: "text", key_type: nil} = column1
    assert %{name: "user_id", type: "text", key_type: "user_id"} = column2
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
    assert Map.drop(immediate_changes, [:name, :creation_status]) == Map.drop(cloak_changes, [:name, :creation_status])
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
    assert [%{name: "user_id", type: "text", key_type: "user_id"}] = updated_table.columns

    assert {:ok, result} = run_query(context.user, "select * from #{new_name}")
    assert result.columns == ~w(user_id)
    assert result.buckets == [%{"occurrences" => 100, "row" => ["*"], "unreliable" => false}]
  end

  test "previous table is dropped if table is renamed on update", context do
    original_name = unique_name()
    new_name = unique_name()

    {:ok, table} = create_table(context.user, original_name, "select user_id, name from users")
    {:ok, cloak_data_source} = Cloak.DataSource.fetch(Manager.data_source().name)
    original_db_name = Cloak.AnalystTable.find(context.user.id, original_name, cloak_data_source).db_name

    assert {:ok, _} = update_table(table.id, context.user, new_name, "select user_id from users")
    assert table_not_in_db?(original_db_name)
  end

  test "failed update when other user", context do
    name = unique_name()
    new_name = unique_name()
    other_user = Manager.create_air_user()

    {:ok, table} = create_table(context.user, name, "select user_id, name from users")
    assert {:error, :not_allowed} = update_table(table.id, other_user, new_name, "select user_id from users")
  end

  test "updating analyst table sets the creation status to pending", context do
    name = unique_name()
    data_source = Manager.data_source()
    {:ok, table} = create_table(context.user, name, "select user_id, name from users")

    :ok = Air.Service.AnalystTable.update_status(context.user.id, data_source.name, name, :succeeded)

    assert soon(
             match?(
               {:ok, %{creation_status: :succeeded}},
               Air.Service.AnalystTable.get_by_name(context.user, data_source, name)
             )
           )

    assert {:ok, updated_table} = update_table(table.id, context.user, name, "select user_id from users group by 1")
    assert updated_table.creation_status == :pending
  end

  test "after the cloak reconnects, it will receive analyst tables from the air", context do
    Air.Repo.delete_all(Air.Schemas.AnalystTable)
    {:ok, cloak_data_source} = Cloak.DataSource.fetch(Manager.data_source().name)

    Cloak.DataSource.Connection.execute!(
      cloak_data_source,
      &cloak_data_source.driver.execute!(&1, "truncate table __ac_analyst_tables_1")
    )

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
    {:ok, cloak_data_source} = Cloak.DataSource.fetch(Manager.data_source().name)
    db_name = Cloak.AnalystTable.find(context.user.id, name, cloak_data_source).db_name

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

  test "views are revalidated after analyst table is deleted", context do
    table_name = unique_name()
    {:ok, table} = create_table(context.user, table_name, "select * from users")

    view_name = unique_name()
    {:ok, view} = Air.Service.View.create(context.user, Manager.data_source(), view_name, "select * from #{table_name}")

    :ok = Air.Service.AnalystTable.delete(table.id, context.user)

    assert soon(Air.Repo.get!(Air.Schemas.View, view.id).broken)
  end

  test "views are revalidated after analyst table is updated", context do
    table_name = unique_name()
    {:ok, table} = create_table(context.user, table_name, "select * from users")

    view_name = unique_name()

    {:ok, view} =
      Air.Service.View.create(
        context.user,
        Manager.data_source(),
        view_name,
        "select user_id, name from #{table_name}"
      )

    {:ok, _table} = update_table(table.id, context.user, table_name, "select user_id from users")

    assert soon(Air.Repo.get!(Air.Schemas.View, view.id).broken)
  end

  test "analyst tables are deleted if the user is deleted" do
    user = Manager.create_air_user()
    {:ok, cloak_data_source} = Cloak.DataSource.fetch(Manager.data_source().name)

    tables =
      Stream.repeatedly(fn ->
        name = unique_name()
        {:ok, table} = create_table(user, name, "select * from users")
        %{id: table.id, db_name: Cloak.AnalystTable.find(user.id, name, cloak_data_source).db_name}
      end)
      |> Enum.take(5)

    Air.Service.User.delete!(user)

    Enum.each(tables, fn table ->
      assert soon(table_not_in_db?(table.db_name), :timer.seconds(5), repeat_wait_time: 10)
      assert soon(is_nil(Air.Repo.get(Air.Schemas.AnalystTable, table.id)), :timer.seconds(5), repeat_wait_time: 10)
    end)
  end

  test "analyst tables are deleted if permissions are revoked" do
    # create new group and add user to it
    group = Air.Service.User.create_group!(%{name: "new_group", admin: false})
    user = Manager.create_air_user(group)

    # load current data source groups (should be just admin)
    data_source = Air.Repo.preload(Manager.data_source(), :groups)
    prev_groups = Enum.map(data_source.groups, & &1.id)

    # allow users from the new group to access data source
    new_groups = [group.id | prev_groups]
    data_source = Air.Service.DataSource.update!(data_source, %{groups: new_groups})

    # create new analyst tables
    {:ok, cloak_data_source} = Cloak.DataSource.fetch(data_source.name)

    tables =
      Stream.repeatedly(fn ->
        name = unique_name()
        {:ok, table} = create_table(user, name, "select * from users")
        %{id: table.id, db_name: Cloak.AnalystTable.find(user.id, name, cloak_data_source).db_name}
      end)
      |> Enum.take(5)

    # revoke access permission to the new group
    Air.Service.DataSource.update!(data_source, %{groups: prev_groups})

    # verify that created tables are deleted
    Enum.each(tables, fn table ->
      assert soon(table_not_in_db?(table.db_name), :timer.seconds(5), repeat_wait_time: 10)
      assert soon(is_nil(Air.Repo.get(Air.Schemas.AnalystTable, table.id)), :timer.seconds(5), repeat_wait_time: 10)
    end)
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
