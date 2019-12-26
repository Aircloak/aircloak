defmodule Air.PsqlServer.ShadowDbTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  alias Air.{TestRepoHelper, TestSocketHelper}
  alias Air.Service.{User, Group, DataSource, View}
  alias Air.Service.LDAP
  import Aircloak.AssertionHelper

  setup_all do
    params = Air.PsqlServer.ShadowDb.connection_params()

    {:ok, pid} =
      Postgrex.start_link(
        hostname: params.host,
        database: params.name,
        username: params.user,
        port: params.port,
        ssl: params.ssl,
        password: params.password
      )

    [shadow_db_conn: pid]
  end

  setup do
    Air.PsqlServer.ShadowDb.SchemaSynchronizer.enable_events()

    on_exit(fn ->
      Air.PsqlServer.ShadowDb.SchemaSynchronizer.disable_events()
    end)
  end

  describe "datasource access change" do
    test "Regular user: Assigning a user to a group, should create shadow dbs for the groups data sources", context do
      group = TestRepoHelper.create_group!() |> Repo.preload(:users)
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!()

      refute shadow_db_exists?(context, user, data_source)

      Group.update!(group, %{users: [user.id]})

      assert soon(shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "Regular user: Assigning a group to a user, should create shadow dbs for the groups data sources",
         context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!()

      refute shadow_db_exists?(context, user, data_source)

      User.update!(user, %{groups: [group.id]})

      assert soon(shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "Regular user: Adding a data source to a group should create shadow dbs for all users in the group", context do
      group = TestRepoHelper.create_group!() |> Repo.preload(:data_sources)
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      data_source = create_data_source!()

      refute shadow_db_exists?(context, user, data_source)

      Group.update!(group, %{data_sources: [data_source.id]})

      assert soon(shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "Regular user: Adding a group to a data source should create shadow dbs for all users in the group", context do
      group = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      data_source = create_data_source!()

      refute shadow_db_exists?(context, user, data_source)

      DataSource.update!(data_source, %{groups: [group.id]})

      assert soon(shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "Regular user: Removing a data source from a group should remove the corresponding shadow dbs", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      DataSource.update!(data_source, %{groups: []})

      assert soon(not shadow_db_exists?(context, user, data_source), 5000), "Shadow db should have been removed"
    end

    test "Regular user: Removing a user from a group should remove the corresponding shadow dbs", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      User.update!(user, %{groups: []})

      assert soon(not shadow_db_exists?(context, user, data_source), 5000), "Shadow db should have been removed"
    end

    test "LDAP user: Assigning a user to a group, should create shadow dbs for the groups data sources", context do
      group = TestRepoHelper.create_group!(%{ldap_dn: "group dn"})
      user = TestRepoHelper.create_user!(%{ldap_dn: "user dn", login: "alice"})
      data_source = create_data_source!(%{groups: [group.id]})

      refute shadow_db_exists?(context, user, data_source)

      LDAP.Sync.sync(
        [%LDAP.User{dn: "user dn", login: "alice", name: "Alice the Magnificent"}],
        [%LDAP.Group{dn: "group dn", name: "group", member_ids: ["alice"]}]
      )

      assert soon(shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "LDAP user: Adding a data source to a group should create shadow dbs for all users in the group", context do
      group = TestRepoHelper.create_group!(%{ldap_dn: "group dn"}) |> Repo.preload(:data_sources)
      user = TestRepoHelper.create_user!(%{ldap_dn: "user dn", login: "alice", groups: [group.id]})
      data_source = create_data_source!()

      refute shadow_db_exists?(context, user, data_source)

      Group.update_data_sources(group, %{data_sources: [data_source.id]})

      assert soon(shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be created after a data source is assigned to group"
    end

    # The inverse of this test, namely removing a group from a data source, is equivalent to the non-LDAP one
    test "LDAP user: Removing a data source from a group should remove the corresponding shadow dbs", context do
      group = TestRepoHelper.create_group!(%{ldap_dn: "group dn"})
      user = TestRepoHelper.create_user!(%{ldap_dn: "user dn", login: "alice", groups: [group.id]})
      data_source = create_data_source!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      Group.load(group.id)
      |> Group.update_data_sources(%{data_sources: []})

      assert soon(not shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be removed after data source is removed from the group"
    end

    test "LDAP user: Removing a user from a group should remove the corresponding shadow dbs", context do
      group = TestRepoHelper.create_group!(%{ldap_dn: "group dn"})
      user = TestRepoHelper.create_user!(%{ldap_dn: "user dn", login: "alice", groups: [group.id]})
      data_source = create_data_source!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      LDAP.Sync.sync(
        [%LDAP.User{dn: "user dn", login: "alice", name: "Alice the Magnificent"}],
        [%LDAP.Group{dn: "group dn", name: "group", member_ids: []}]
      )

      assert soon(not shadow_db_exists?(context, user, data_source), 5000),
             "Shadow db should be removed after data source is removed from the group"
    end
  end

  describe "deletion" do
    test "If a user is deleted, then all related shadow dbs should be removed", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      User.delete!(user)

      assert soon(not shadow_db_exists?(context, user, data_source), 5000), "Shadow db should have been removed"
    end

    test "If a group is deleted, the orphaned shadow dbs should be removed", context do
      group1 = TestRepoHelper.create_group!()
      group2 = TestRepoHelper.create_group!()

      data_source1 = create_data_source!(%{groups: [group1.id]})
      data_source2 = create_data_source!(%{groups: [group1.id, group2.id]})

      user = TestRepoHelper.create_user!(%{groups: [group1.id, group2.id]})

      trigger_shadow_db_creation(context, user, data_source1)
      trigger_shadow_db_creation(context, user, data_source2)

      Group.delete!(group1)

      assert soon(not shadow_db_exists?(context, user, data_source1), 5000),
             "Orphaned shadow db should have been removed"

      assert shadow_db_exists?(context, user, data_source2), "Shadow db for still accessible data source should remain"
    end

    test "If a data source is deleted then all related shadow dbs should be removed", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      DataSource.delete!(data_source, fn -> :ok end, fn -> :ok end)

      assert soon(not shadow_db_exists?(context, user, data_source), 5000), "Shadow db should have been removed"
    end
  end

  describe "selectables" do
    test "Creating a view should create the corresponding table in the users shadow db", context do
      {view, user, data_source, _socket} = create_view(context)
      assert soon(shadow_db_has_table?(user, data_source, view.name))
      cleanup(user, data_source)
    end

    test "Altering a view should update the corresponding table in the users shadow db", context do
      {view, user, data_source, socket} = create_view(context)
      assert soon(shadow_db_has_table?(user, data_source, view.name))

      task = Task.async(fn -> View.update(view.id, user, "updated_view_name", "some sql") end)
      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)
      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

      assert {:ok, _} = Task.await(task)
      assert soon(shadow_db_has_table?(user, data_source, "updated_view_name"))
      cleanup(user, data_source)
    end

    test "Removing a view should remove the corresponding table in the users shadow db", context do
      {view, user, data_source, socket} = create_view(context)
      assert soon(shadow_db_has_table?(user, data_source, view.name))

      task = Task.async(fn -> View.delete(view.id, user) end)
      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

      assert :ok = Task.await(task)
      assert soon(not shadow_db_has_table?(user, data_source, view.name))
      cleanup(user, data_source)
    end

    test "Creating an analyst table should, upon completion, create the corresponding table in the shadow db",
         context do
      {table, user, data_source, _socket} = create_analyst_table(context)
      assert soon(shadow_db_has_table?(user, data_source, table.name))
      cleanup(user, data_source)
    end

    test "Altering an analyst table should, upon completion, update the corresponding table in the shadow db",
         context do
      {table, user, data_source, socket} = create_analyst_table(context)
      old_name = table.name
      new_name = "my updated table 1"

      task =
        Task.async(fn ->
          Air.Service.AnalystTable.update(table.id, user, new_name, "some sql")
        end)

      TestSocketHelper.respond_to_create_or_update_analyst_table(socket)
      assert {:ok, table} = Task.await(task)

      assert soon(not shadow_db_has_table?(user, data_source, old_name))
      assert soon(shadow_db_has_table?(user, data_source, new_name))

      cleanup(user, data_source)
    end

    test "Removing an analyst table should remove the corresponding table in the shadow db", context do
      {table, user, data_source, socket} = create_analyst_table(context)

      assert soon(shadow_db_has_table?(user, data_source, table.name))

      task =
        Task.async(fn ->
          Air.Service.AnalystTable.delete(table.id, user)
        end)

      TestSocketHelper.respond_to_drop_analyst_table(socket)

      :ok = Task.await(task)

      assert soon(not shadow_db_has_table?(user, data_source, table.name))

      cleanup(user, data_source)
    end

    test "Other users should not have access to a users selectables through shadow db"
    test "Recreating a shadow db based on schema changes from cloak should also include selectables"
  end

  describe "psql interface" do
    test "Views should be listed amongst tables"
    test "Analyst tables should be listed amongst tables"
  end

  defp shadow_db_exists?(context, user, data_source) do
    result =
      Postgrex.query!(
        context.shadow_db_conn,
        "SELECT distinct datname FROM pg_catalog.pg_database WHERE datname LIKE 'aircloak_shadow%'",
        []
      )

    result.rows
    |> List.flatten()
    |> Enum.member?(Air.PsqlServer.ShadowDb.db_name(user, data_source.name))
  end

  defp shadow_db_has_table?(user, data_source, table) do
    db_name = Air.PsqlServer.ShadowDb.db_name(user, data_source.name)
    parent = self()

    Task.start(fn ->
      Air.PsqlServer.ShadowDb.Connection.execute!(db_name, fn conn ->
        {_columns, rows} =
          Air.PsqlServer.ShadowDb.Connection.query!(
            conn,
            "SELECT table_name FROM information_schema.tables where table_schema=$1",
            ["public"]
          )

        send(parent, {:rows, rows})
      end)
    end)

    receive do
      {:rows, rows} ->
        Enum.any?(rows, fn [t] -> t == table end)
    after
      1000 -> false
    end
  end

  defp create_analyst_table(context) do
    group = TestRepoHelper.create_group!()
    data_source = create_data_source!(%{groups: [group.id]})
    user = TestRepoHelper.create_user!(%{groups: [group.id]})
    trigger_shadow_db_creation(context, user, data_source)
    socket = data_source_socket(data_source)
    name = "my table 1"

    assert not shadow_db_has_table?(user, data_source, name)

    task =
      Task.async(fn ->
        Air.Service.AnalystTable.create(user, data_source, name, "some sql")
      end)

    TestSocketHelper.respond_to_create_or_update_analyst_table(socket)

    assert {:ok, table} = Task.await(task)

    {table, user, data_source, socket}
  end

  defp create_view(context) do
    group = TestRepoHelper.create_group!()
    data_source = create_data_source!(%{groups: [group.id]})
    user = TestRepoHelper.create_user!(%{groups: [group.id]})
    trigger_shadow_db_creation(context, user, data_source)
    socket = data_source_socket(data_source)
    name = "my view 1"

    assert not shadow_db_has_table?(user, data_source, name)

    task =
      Task.async(fn ->
        View.create(user, data_source, name, "some sql", skip_revalidation: true)
      end)

    TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

    assert {:ok, view} = Task.await(task)
    {view, user, data_source, socket}
  end

  defp create_data_source!(params \\ %{}) do
    tables =
      Jason.encode!([
        %{columns: [%{key_type: "user_id", name: "uid", type: "integer", user_id: true}], id: "table_name"}
      ])

    TestRepoHelper.create_data_source!(Map.merge(%{tables: tables}, params))
  end

  defp trigger_shadow_db_creation(context, user, data_source) do
    Air.PsqlServer.ShadowDb.update(user, data_source.name)

    assert soon(shadow_db_exists?(context, user, data_source), 5000),
           "Shadow db should have been created or updated"
  end

  defp data_source_socket(data_source) do
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})

    TestSocketHelper.join!(socket, "main", %{
      data_sources: [%{name: data_source.name, tables: []}]
    })

    socket
  end

  defp cleanup(user, data_source) do
    # Async activities may linger after tests, which results in confusing Ecto errors.
    # We wait for ShadowDb servers to handle all messages before concluding tests.
    Air.PsqlServer.ShadowDb.SchemaSynchronizer.flush()
    Air.PsqlServer.ShadowDb.Manager.wait_until_initialized(user, data_source.name)
  end

  defp revalidation_success(names) do
    columns = [%{name: "foo", type: "integer", key_type: "user_id"}, %{name: "bar", type: "text", key_type: nil}]
    Enum.map(names, &%{name: &1, columns: columns, valid: true})
  end
end
