defmodule Air.Service.ViewTest do
  # because of shared mode
  use Air.SchemaCase, async: false

  alias Air.{Service.View, Service.User, TestRepoHelper, TestSocketHelper}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    Air.TestRepoHelper.create_privacy_policy!()

    g1 = TestRepoHelper.create_group!()
    g2 = TestRepoHelper.create_group!()
    g3 = TestRepoHelper.create_group!()

    u1 = TestRepoHelper.create_user!(%{groups: [g1.id, g2.id]}) |> accept_privacy_policy_and_reload()
    u2 = TestRepoHelper.create_user!(%{groups: [g2.id, g3.id]}) |> accept_privacy_policy_and_reload()
    u3 = TestRepoHelper.create_user!(%{groups: [g3.id]}) |> accept_privacy_policy_and_reload()

    ds1 = TestRepoHelper.create_data_source!(%{groups: [g1.id]})
    ds2 = TestRepoHelper.create_data_source!(%{groups: [g2.id]})

    v1 = insert_view(ds1, u1, "view_1")
    v2 = insert_view(ds1, u1, "view_2")
    v3 = insert_view(ds2, u1, "view_3")
    v4 = insert_view(ds2, u2, "view_4")

    %{u1: u1, u2: u2, u3: u3, ds1: ds1, ds2: ds2, v1: v1, v2: v2, v3: v3, v4: v4}
  end

  test(
    "fetching the changeset for the view",
    context,
    do: assert(Ecto.Changeset.apply_changes(View.changeset(context.v1.id)) == context.v1)
  )

  test "fetching views for the user and the datasource", context do
    assert View.all(context.u1, context.ds1) |> Enum.sort_by(& &1.id) == [context.v1, context.v2]
    assert View.all(context.u1, context.ds2) == [context.v3]

    assert View.all(context.u2, context.ds1) == []
    assert View.all(context.u2, context.ds2) == [context.v4]

    assert View.all(context.u3, context.ds1) == []
    assert View.all(context.u3, context.ds2) == []
  end

  test "fetching the view map", context do
    assert View.user_views_map(context.u1, context.ds1.id) == %{
             context.v1.name => context.v1.sql,
             context.v2.name => context.v2.sql
           }
  end

  test "cloak not available error", context do
    assert {:error, %Ecto.Changeset{errors: errors}} = View.create(context.u1, context.ds1, "name", "sql")

    assert {error, _} = Keyword.fetch!(errors, :sql)

    assert error ==
             "The view cannot be saved because no cloak is currently available for the given data source. " <>
               "Please contact your administrator."
  end

  describe "creating a view" do
    test "success", context do
      socket = data_source_socket(context.ds1)

      task =
        Task.async(fn ->
          View.create(context.u1, context.ds1, "some view", "some sql", skip_revalidation: true)
        end)

      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

      assert {:ok, %{result_info: %{columns: ["some", "columns"]}}} = Task.await(task)
    end

    test "failure", context do
      socket = data_source_socket(context.ds1)

      task = Task.async(fn -> View.create(context.u1, context.ds1, "some view", "some sql") end)

      TestSocketHelper.respond_to_validate_views!(socket, fn _ ->
        [%{name: "some view", valid: false, field: :sql, error: "some error"}]
      end)

      assert {:error, %{valid?: false, errors: [sql: {"some error", []}]}} = Task.await(task)
    end

    test "revalidating other views", context do
      socket = data_source_socket(context.ds1)

      task =
        Task.async(fn ->
          View.create(
            context.u1,
            context.ds1,
            "some view",
            "some sql",
            revalidation_timeout: :timer.seconds(1)
          )
        end)

      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

      TestSocketHelper.respond_to_validate_views!(socket, fn _ ->
        [%{name: context.v2.name, valid: false, field: :sql, error: "some error"}]
      end)

      Task.await(task)

      assert Repo.get(Air.Schemas.View, context.v2.id).broken
    end
  end

  describe "updating a view" do
    test "success", context do
      View.subscribe_to(:revalidated_views)
      socket = data_source_socket(context.ds1)

      task = Task.async(fn -> View.update(context.v1.id, context.u1, "some view", "some sql") end)
      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)
      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

      view_id = context.v1.id
      assert {:ok, %{id: ^view_id}} = Task.await(task)

      assert %{
               name: "some view",
               sql: "some sql",
               result_info: %{"columns" => ["some", "columns"]}
             } = Repo.get(Air.Schemas.View, context.v1.id)

      assert_receive {:revalidated_views, _}
    end

    test "failure", context do
      socket = data_source_socket(context.ds1)

      task = Task.async(fn -> View.update(context.v1.id, context.u1, "some view", "some sql") end)

      TestSocketHelper.respond_to_validate_views!(socket, fn _ ->
        [%{name: "some view", valid: false, field: :sql, error: "some error"}]
      end)

      assert {:error, %{valid?: false, errors: [sql: {"some error", []}]}} = Task.await(task)
      assert Repo.get(Air.Schemas.View, context.v1.id).name == context.v1.name
    end

    test "revalidating other views", context do
      socket = data_source_socket(context.ds1)

      task =
        Task.async(fn ->
          View.update(
            context.v1.id,
            context.u1,
            "some view",
            "some sql",
            revalidation_timeout: :timer.seconds(1)
          )
        end)

      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

      TestSocketHelper.respond_to_validate_views!(socket, fn _ ->
        [%{name: context.v2.name, valid: false, field: :sql, error: "some error"}]
      end)

      Task.await(task)

      assert Repo.get(Air.Schemas.View, context.v2.id).broken
    end
  end

  describe "deleting a view" do
    test "success", context do
      View.subscribe_to(:revalidated_views)
      socket = data_source_socket(context.ds1)

      task = Task.async(fn -> View.delete(context.v1.id, context.u1) end)
      TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

      assert :ok = Task.await(task)
      assert nil == Repo.get(Air.Schemas.View, context.v1.id)

      assert_receive {:revalidated_views, _}
    end

    test "revalidating other views", context do
      socket = data_source_socket(context.ds1)

      task =
        Task.async(fn ->
          View.delete(context.v1.id, context.u1, revalidation_timeout: :timer.seconds(1))
        end)

      TestSocketHelper.respond_to_validate_views!(socket, fn _ ->
        [%{name: context.v2.name, valid: false, field: :sql, error: "some error"}]
      end)

      Task.await(task)

      assert Repo.get(Air.Schemas.View, context.v2.id).broken
    end
  end

  test "revalidating all views", context do
    socket = data_source_socket(context.ds2)
    View.subscribe_to(:revalidated_views)
    View.revalidate_all_views(context.ds2)

    TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)
    TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

    assert_receive {:revalidated_views, m1}
    assert_receive {:revalidated_views, m2}
    refute_receive {:revalidated_views, _}
    assert Enum.all?([m1, m2], &(&1.data_source_id == context.ds2.id))

    assert Enum.sort(Enum.map([m1, m2], & &1.user_id)) == Enum.sort([context.u1.id, context.u2.id])
  end

  test "[Issue #1948] multiple users have views with the same name", context do
    _same_name_view = insert_view(context.ds2, context.u2, context.v3.name)

    socket = data_source_socket(context.ds2)
    View.subscribe_to(:revalidated_views)
    View.revalidate_all_views(context.ds2)

    TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)
    TestSocketHelper.respond_to_validate_views!(socket, &revalidation_success/1)

    user1_id = context.u1.id
    assert_receive {:revalidated_views, %{user_id: ^user1_id}}
    user2_id = context.u2.id
    assert_receive {:revalidated_views, %{user_id: ^user2_id}}
  end

  defp insert_view(data_source, user, name),
    do:
      %Air.Schemas.View{}
      |> Ecto.Changeset.cast(
        %{
          user_id: user.id,
          data_source_id: data_source.id,
          name: name,
          sql: "sql for #{name}",
          result_info: %{}
        },
        ~w(name sql user_id data_source_id result_info)a
      )
      |> Repo.insert!()

  defp data_source_socket(data_source) do
    socket = TestSocketHelper.connect!(%{cloak_name: "cloak_1"})

    TestSocketHelper.join!(socket, "main", %{
      data_sources: [%{name: data_source.name, tables: []}]
    })

    socket
  end

  defp revalidation_success(names), do: Enum.map(names, &%{name: &1, columns: ["some", "columns"], valid: true})

  defp accept_privacy_policy_and_reload(user) do
    User.accept_privacy_policy(user)
    User.load(user.id)
  end
end
