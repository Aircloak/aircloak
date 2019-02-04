defmodule Air.Service.DataSourceTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  alias Air.Service.{User, DataSource}
  alias Air.Schemas
  alias Air.TestRepoHelper

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})

    g1 = TestRepoHelper.create_group!()
    g2 = TestRepoHelper.create_group!()
    g3 = TestRepoHelper.create_group!()

    user1 = TestRepoHelper.create_user!(%{groups: [g1.id, g2.id]})
    user2 = TestRepoHelper.create_user!(%{groups: [g2.id, g3.id]})
    user3 = TestRepoHelper.create_user!(%{groups: [g3.id]})

    ds1 = TestRepoHelper.create_data_source!(%{groups: [g1.id]})
    ds2 = TestRepoHelper.create_data_source!(%{groups: [g2.id]})

    %{
      user1: user1,
      user2: user2,
      user3: user3,
      ds1: ds1,
      ds2: ds2,
      group1: g1,
      group2: g2,
      group3: g3
    }
  end

  test "mark_as_pending_delete!",
    do: assert(DataSource.mark_as_pending_delete!(TestRepoHelper.create_data_source!()).pending_delete)

  test "all", context do
    expected_datasources = Enum.sort([context.ds1.id, context.ds2.id])

    actual_datasources =
      DataSource.all()
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert expected_datasources == actual_datasources
  end

  test "all does not include those pending delete", context do
    DataSource.mark_as_pending_delete!(context.ds2)
    assert [context.ds1.id] == Enum.map(DataSource.all(), & &1.id)
  end

  test "count" do
    initial_count = DataSource.count()

    Enum.each(1..5, fn _ -> TestRepoHelper.create_data_source!() end)
    assert DataSource.count() == initial_count + 5
  end

  test "count does not include pending deletes" do
    initial_count = DataSource.count()

    Enum.each(1..5, fn _ -> TestRepoHelper.create_data_source!() end)
    DataSource.mark_as_pending_delete!(TestRepoHelper.create_data_source!())

    assert DataSource.count() == initial_count + 5
  end

  test "fetching user's data sources", context do
    expected_datasources = Enum.sort([context.ds1.id, context.ds2.id])

    actual_datasources =
      DataSource.for_user(context.user1)
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert expected_datasources == actual_datasources
    assert Enum.map(DataSource.for_user(context.user2), & &1.id) == [context.ds2.id]
    assert Enum.map(DataSource.for_user(context.user3), & &1.id) == []
  end

  test "fetching user's data sources do not include ones pending delete", context do
    DataSource.mark_as_pending_delete!(context.ds1)
    expected_datasources = [context.ds2.id]

    actual_datasources =
      DataSource.for_user(context.user1)
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert expected_datasources == actual_datasources
    assert Enum.map(DataSource.for_user(context.user2), & &1.id) == [context.ds2.id]
    assert Enum.map(DataSource.for_user(context.user3), & &1.id) == []
  end

  test "fetching user's data sources - no duplicates, despite being allowed through multiple groups" do
    g1 = TestRepoHelper.create_group!()
    g2 = TestRepoHelper.create_group!()
    user = TestRepoHelper.create_user!(%{groups: [g1.id, g2.id]})
    ds = TestRepoHelper.create_data_source!(%{groups: [g1.id, g2.id]})
    expected_datasources = Enum.sort([ds.id])

    actual_datasources =
      DataSource.for_user(user)
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert expected_datasources == actual_datasources
  end

  test "fetching available data source", context do
    assert {:ok, data_source} = DataSource.fetch_as_user({:id, context.ds1.id}, context.user1)
    assert data_source.id == context.ds1.id
  end

  test "fetching available data source with a name", context do
    assert {:ok, data_source} = DataSource.fetch_as_user({:name, context.ds1.name}, context.user1)
    assert data_source.id == context.ds1.id
  end

  test(
    "fetching unavailable data source",
    context,
    do: assert({:error, :unauthorized} == DataSource.fetch_as_user({:id, context.ds1.id}, context.user3))
  )

  test "fetching user's history", context do
    queries = Enum.map(1..5, fn _ -> create_query(context.user1, context.ds2) end)
    _other_user_queries = Enum.map(1..5, fn _ -> create_query(context.user2, context.ds2) end)

    for count <- 1..6 do
      assert {:ok, history} =
               DataSource.history(
                 {:id, context.ds2.id},
                 context.user1,
                 :http,
                 count,
                 NaiveDateTime.utc_now()
               )

      assert Enum.map(history, & &1.id) == queries |> Enum.reverse() |> Enum.take(min(5, count)) |> Enum.map(& &1.id)
    end
  end

  test "fetching history filters by context", context do
    Enum.map(1..5, fn _ -> create_query(context.user1, context.ds2, %{context: :psql}) end)

    assert {:ok, []} ==
             DataSource.history(
               {:id, context.ds2.id},
               context.user1,
               :http,
               1,
               NaiveDateTime.utc_now()
             )

    assert {:ok, [_ | _]} =
             DataSource.history(
               {:id, context.ds2.id},
               context.user1,
               :psql,
               1,
               NaiveDateTime.utc_now()
             )
  end

  test(
    "fetching history for the unavailable data source",
    context,
    do:
      assert(
        {:error, :unauthorized} ==
          DataSource.history(
            {:id, context.ds2.id},
            context.user3,
            :http,
            1,
            NaiveDateTime.utc_now()
          )
      )
  )

  test "fetching last query", context do
    queries = Enum.map(1..5, fn _ -> create_query(context.user1, context.ds2) end)
    _other_user_queries = Enum.map(1..5, fn _ -> create_query(context.user2, context.ds2) end)

    assert {:ok, query} = DataSource.last_query({:id, context.ds2.id}, context.user1, :http)
    assert query.id == List.last(queries).id
  end

  test "fetching last query filters by context", context do
    Enum.map(1..5, fn _ -> create_query(context.user1, context.ds2, %{context: :psql}) end)

    assert {:ok, nil} == DataSource.last_query({:id, context.ds2.id}, context.user1, :http)
    assert {:ok, query} = DataSource.last_query({:id, context.ds2.id}, context.user1, :psql)
    assert query != nil
  end

  test(
    "fetching last query when there are no queries",
    context,
    do: assert({:ok, nil} == DataSource.last_query({:id, context.ds2.id}, context.user1, :http))
  )

  test(
    "fetching last query for the unavailable data source",
    context,
    do: assert({:error, :unauthorized} == DataSource.last_query({:id, context.ds2.id}, context.user3, :http))
  )

  test "returns a list of data sources given their names" do
    ds1 = TestRepoHelper.create_data_source!()
    ds2 = TestRepoHelper.create_data_source!()

    expected =
      [ds1, ds2]
      |> Enum.map(& &1.name)
      |> Enum.sort()

    assert DataSource.by_names([ds1.name, ds2.name])
           |> Enum.map(& &1.name)
           |> Enum.sort() == expected
  end

  describe "create or update data source" do
    test "should update existing data source" do
      table = %{id: "table_id", columns: []}
      name = "new_name"
      data_source = DataSource.create_or_update_data_source(%{name: name, tables: [table]})
      assert data_source.id == DataSource.create_or_update_data_source(%{name: name, tables: [table]}).id
    end

    test "should create new data source if none exists" do
      table = %{id: "table_id", columns: []}
      name = "new_name"
      refute Repo.all(Air.Schemas.DataSource) |> Enum.any?(&(&1.name == name))
      assert %Air.Schemas.DataSource{} = DataSource.create_or_update_data_source(%{name: name, tables: [table]})
    end

    test "should store the database host" do
      data_source = DataSource.create_or_update_data_source(%{name: "new_name", tables: [], database_host: "a_host"})
      assert data_source.database_host == "a_host"
    end

    test "should precompute and store isolator status" do
      tables = [
        %{
          id: "table_id",
          columns: [
            %{name: "col1", shadow_table: :ok, isolated: true},
            %{name: "col2", shadow_table: :ok, isolated: false},
            %{name: "failure", shadow_table: :ok, isolated: :failed},
            %{name: "col3", shadow_table: :ok, isolated: :other}
          ]
        }
      ]

      data_source = DataSource.create_or_update_data_source(%{name: "new_name", tables: tables})

      assert data_source.columns_count == 4
      assert data_source.isolated_computed_count == 2
      assert data_source.isolated_failed == ["table_id.failure"]
    end

    test "should compute and store shadow table status" do
      tables = [
        %{
          id: "table_id",
          columns: [
            %{name: "col1", isolated: true, shadow_table: :ok},
            %{name: "col2", isolated: true, shadow_table: :ok},
            %{name: "failure", isolated: true, shadow_table: :failed},
            %{name: "col3", isolated: true, shadow_table: :other}
          ]
        }
      ]

      data_source = DataSource.create_or_update_data_source(%{name: "new_name", tables: tables})

      assert data_source.columns_count == 4
      assert data_source.shadow_tables_computed_count == 2
      assert data_source.shadow_tables_failed == ["table_id.failure"]
    end

    test "[Issue #3208] does not crash for old cloaks" do
      tables = [
        %{
          id: "table_id",
          columns: [%{name: "col1"}]
        }
      ]

      data_source = DataSource.create_or_update_data_source(%{name: "new_name", tables: tables})

      assert data_source.columns_count == 1
      assert data_source.shadow_tables_computed_count == 1
      assert data_source.isolated_computed_count == 1
      assert data_source.shadow_tables_failed == []
      assert data_source.isolated_failed == []
    end
  end

  test "should be able to tell when a data source is available" do
    data_source_name = TestRepoHelper.create_and_register_data_source()
    assert DataSource.available?(data_source_name)
  end

  test "should be able to tell when a data source is not available" do
    refute DataSource.available?("some_id")
  end

  test "required attributes",
    do: assert(errors_on(&DataSource.create/1, %{}) == [name: "can't be blank", tables: "can't be blank"])

  test "validates uniqueness of name" do
    DataSource.create!(%{name: "baz", tables: "[]"})

    assert errors_on(&DataSource.create/1, %{name: "baz", tables: "[]"}) == [name: "has already been taken"]
  end

  test "a data_source can have many groups" do
    group1 = TestRepoHelper.create_group!()
    group2 = TestRepoHelper.create_group!()
    data_source = TestRepoHelper.create_data_source!(%{groups: [group1.id, group2.id]})
    assert [group1.id, group2.id] == Enum.map(data_source.groups, & &1.id) |> Enum.sort()
  end

  describe "delete!" do
    test "deleting a data source, doesn't delete the group" do
      group = TestRepoHelper.create_group!()
      data_source = TestRepoHelper.create_data_source!(%{groups: [group.id]})

      me = self()
      DataSource.delete!(data_source, fn -> send(me, :success) end, fn -> :ignore end)

      receive do
        :success -> refute nil == Air.Service.User.load_group(group.id)
      end
    end

    test "deleting a data source deletes its views" do
      user = TestRepoHelper.create_user!()
      data_source = TestRepoHelper.create_data_source!()
      TestRepoHelper.create_view!(user, data_source)

      me = self()
      DataSource.delete!(data_source, fn -> send(me, :success) end, fn -> :ignore end)

      receive do
        :success -> assert [] = Air.Service.View.all(user, data_source)
      end
    end

    test "deletes queries with result chunks" do
      data_source = TestRepoHelper.create_data_source!()
      query = TestRepoHelper.create_query!(TestRepoHelper.create_user!(), %{data_source_id: data_source.id})
      TestRepoHelper.send_query_result(query.id, %{}, [%{occurrences: 1, row: ["some", "data"]}])

      me = self()
      DataSource.delete!(data_source, fn -> send(me, :success) end, fn -> :ignore end)

      receive do
        :success -> assert is_nil(Repo.get(Air.Schemas.Query, query.id))
      end
    end
  end

  test "replacing a group for a data_source, removes the old relationship" do
    group1 = TestRepoHelper.create_group!()
    group2 = TestRepoHelper.create_group!()
    data_source = TestRepoHelper.create_data_source!(%{groups: [group1.id]})
    DataSource.update(data_source, %{groups: [group2.id]})
    assert [group2.id] == DataSource.by_name(data_source.name).groups |> Enum.map(& &1.id)
  end

  test "retrieving users for a data source" do
    g1 = TestRepoHelper.create_group!()
    g2 = TestRepoHelper.create_group!()
    g3 = TestRepoHelper.create_group!()

    u1 = TestRepoHelper.create_user!(%{groups: [g1.id, g2.id]})
    u2 = TestRepoHelper.create_user!(%{groups: [g2.id, g3.id]})
    _u3 = TestRepoHelper.create_user!(%{groups: [g3.id]})
    data_source = TestRepoHelper.create_data_source!(%{groups: [g1.id, g2.id]})

    assert DataSource.users(data_source) |> Enum.map(& &1.id) |> Enum.sort() ==
             [u1, u2] |> Enum.map(& &1.id) |> Enum.sort()
  end

  describe ".tables" do
    test "should list available tables" do
      tables = [%{id: "table_id", columns: []}]
      name = "new_name"
      data_source = DataSource.create_or_update_data_source(%{name: name, tables: tables})
      assert [%{"id" => "table_id", "columns" => []}] == Schemas.DataSource.tables(data_source)
    end
  end

  describe ".selectables" do
    setup do
      tables = []
      name = "new_name"
      data_source = DataSource.create_or_update_data_source(%{name: name, tables: tables})

      group = TestRepoHelper.create_group!()
      user = TestRepoHelper.create_user!(%{groups: [group.id]})

      {:ok, user: user, data_source: data_source}
    end

    test "should list views as part of tables", context do
      view_name = "view1"

      %Air.Schemas.View{}
      |> Ecto.Changeset.cast(
        %{
          user_id: context[:user].id,
          data_source_id: context[:data_source].id,
          name: view_name,
          sql: "sql for #{view_name}",
          result_info: %{"columns" => ["foo", "bar"]}
        },
        ~w(name sql user_id data_source_id result_info)a
      )
      |> Repo.insert!()

      assert [%{id: ^view_name, view: true}] = DataSource.selectables(context[:user], context[:data_source])
    end
  end

  describe ".add_preconfigured_datasource" do
    test "creates a data source and gives the users access to it" do
      user = TestRepoHelper.create_user!()

      data = %{
        logins: [User.main_login(user)],
        name: "foobar",
        group_name: "foobar_group"
      }

      assert {:ok, data_source} = DataSource.add_preconfigured_datasource(data)
      assert [User.main_login(user)] == data_source |> DataSource.users() |> Enum.map(&User.main_login/1)
    end

    test "ignores missing users" do
      user = TestRepoHelper.create_user!()
      login = User.main_login(user)

      data = %{
        logins: [login, "missing", "user"],
        name: "foobar",
        group_name: "foobar_group"
      }

      assert {:ok, data_source} = DataSource.add_preconfigured_datasource(data)
      assert [^login] = data_source |> DataSource.users() |> Enum.map(&User.main_login/1)
    end

    test "names the group used for assigning users as configured" do
      user = TestRepoHelper.create_user!()

      data = %{
        logins: [User.main_login(user), "missing", "user"],
        name: "foobar",
        group_name: "foobar_group"
      }

      assert {:ok, data_source} = DataSource.add_preconfigured_datasource(data)
      assert [group] = data_source.groups
      assert group.name == "foobar_group"
    end

    test "the created group does not have give users admin rights" do
      user = TestRepoHelper.create_user!()

      data = %{
        logins: [User.main_login(user)],
        name: "foobar",
        group_name: "foobar_group"
      }

      assert {:ok, data_source} = DataSource.add_preconfigured_datasource(data)
      assert [group] = data_source.groups
      refute group.admin
    end

    test "fails if data source exists" do
      data_source = TestRepoHelper.create_data_source!()
      user = TestRepoHelper.create_user!()

      data = %{
        logins: [User.main_login(user)],
        name: data_source.name,
        group_name: "foobar_group"
      }

      assert {:error, :data_source_exists} == DataSource.add_preconfigured_datasource(data)
    end

    test "fails if the group already exists" do
      user = TestRepoHelper.create_user!()
      group = TestRepoHelper.create_group!()

      data = %{
        logins: [User.main_login(user)],
        name: "foobar",
        group_name: group.name
      }

      assert {:error, :group_exists} == DataSource.add_preconfigured_datasource(data)
    end

    test "fails if no users would be given access to the data source" do
      data = %{
        logins: ["bogus", "logins"],
        name: "foobar",
        group_name: "foobar_group"
      }

      assert {:error, :no_users} == DataSource.add_preconfigured_datasource(data)
    end
  end

  defp create_query(user, data_source, additional_data \\ %{}),
    do:
      TestRepoHelper.create_query!(
        user,
        Map.merge(%{statement: "query content", data_source_id: data_source.id}, additional_data)
      )

  defp errors_on(fun, changes) do
    assert {:error, changeset} = fun.(changes)

    changeset
    |> Ecto.Changeset.traverse_errors(&AirWeb.ErrorHelpers.translate_error/1)
    |> Enum.flat_map(fn {key, errors} -> for msg <- errors, do: {key, msg} end)
  end
end
