defmodule Air.PsqlServer.ShadowDbTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  alias Air.TestRepoHelper
  alias Air.Service.{User, Group, DataSource}
  alias Air.Service.LDAP.{Sync, User, Group}
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

  describe "datasource access change" do
    test "Regular user: Assigning a user to a group, should create shadow dbs for the groups data sources"

    test "Regular user: Assigning a group to a user, should create shadow dbs for the groups data sources",
         context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!()

      refute shadow_db_exists(context, user, data_source)

      User.update!(user, %{groups: [group.id]})

      assert soon(shadow_db_exists(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "Regular user: Adding a data source to a group should create shadow dbs for all users in the group"

    test "Regular user: Adding a group to a data source should create shadow dbs for all users in the group", context do
      group = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      data_source = create_data_source!()

      refute shadow_db_exists(context, user, data_source)

      DataSource.update!(data_source, %{groups: [group.id]})

      assert soon(shadow_db_exists(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "Regular user: Removing a data source from a group should remove the corresponding shadow dbs", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      DataSource.update!(data_source, %{groups: []})

      assert soon(not shadow_db_exists(context, user, data_source), 5000), "Shadow db should have been removed"
    end

    test "Regular user: Removing a user from a group should remove the corresponding shadow dbs", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      User.update!(user, %{groups: []})

      assert soon(not shadow_db_exists(context, user, data_source), 5000), "Shadow db should have been removed"
    end

    test "LDAP user: Assigning a user to a group, should create shadow dbs for the groups data sources", context do
      group = TestRepoHelper.create_group!(%{ldap_dn: "group dn"})
      user = TestRepoHelper.create_user!(%{ldap_dn: "user dn", login: "alice"})
      data_source = create_data_source!(%{groups: [group.id]})

      refute shadow_db_exists(context, user, data_source)

      Sync.sync(
        [%User{dn: "user dn", login: "alice", name: "Alice the Magnificent"}],
        [%Group{dn: "group dn", name: "group", member_ids: ["alice"]}]
      )

      assert soon(shadow_db_exists(context, user, data_source), 5000),
             "Shadow db should be created after user is assigned to group"
    end

    test "LDAP user: Adding a data source to a group should create shadow dbs for all users in the group"
    test "LDAP user: Removing a data source from a group should remove the corresponding shadow dbs"
    test "LDAP user: Removing a user from a group should remove the corresponding shadow dbs"
  end

  describe "deletion" do
    test "If a user is deleted, then all related shadow dbs should be removed", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      User.delete!(user)

      assert soon(not shadow_db_exists(context, user, data_source), 5000), "Shadow db should have been removed"
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

      assert soon(not shadow_db_exists(context, user, data_source1), 5000),
             "Orphaned shadow db should have been removed"

      assert shadow_db_exists(context, user, data_source2), "Shadow db for still accessible data source should remain"
    end

    test "If a data source is deleted then all related shadow dbs should be removed", context do
      group = TestRepoHelper.create_group!()
      data_source = create_data_source!(%{groups: [group.id]})
      user = TestRepoHelper.create_user!(%{groups: [group.id]})
      trigger_shadow_db_creation(context, user, data_source)

      DataSource.delete!(data_source, fn -> :ok end, fn -> :ok end)

      assert soon(not shadow_db_exists(context, user, data_source), 5000), "Shadow db should have been removed"
    end
  end

  describe "selectables" do
    test "Creating a view should create the corresponding table in the users shadow db"
    test "Altering a view should update the corresponding table in the users shadow db"
    test "Removing a view should remove the corresponding table in the users shadow db"

    test "Creating an analyst table should, upon completion, create the corresponding table in the shadow db"
    test "Altering an analyst table should, upon completion, update the corresponding table in the shadow db"
    test "Removing an analyst table should remove the corresponding table in the shadow db"

    test "Other users should not have access to a users selectables through shadow db"
    test "Recreating a shadow db based on schema changes from cloak should also include selectables"
  end

  describe "psql interface" do
    test "Views should be listed amongst tables"
    test "Analyst tables should be listed amongst tables"
  end

  defp shadow_db_exists(context, user, data_source) do
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

  def create_data_source!(params \\ %{}) do
    tables =
      Jason.encode!([
        %{columns: [%{key_type: "user_id", name: "uid", type: "integer", user_id: true}], id: "table_name"}
      ])

    TestRepoHelper.create_data_source!(Map.merge(%{tables: tables}, params))
  end

  def trigger_shadow_db_creation(context, user, data_source) do
    Air.PsqlServer.ShadowDb.update(user, data_source.name)

    assert soon(shadow_db_exists(context, user, data_source), 5000),
           "Shadow db should have been created or updated"
  end
end
