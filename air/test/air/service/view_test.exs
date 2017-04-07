defmodule Air.Service.ViewTest do
  use Air.SchemaCase, async: false # because of shared mode

  alias Air.{Service.View, TestRepoHelper}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})

    g1 = TestRepoHelper.create_group!()
    g2 = TestRepoHelper.create_group!()
    g3 = TestRepoHelper.create_group!()

    u1 = TestRepoHelper.create_user!(%{groups: [g1.id, g2.id]})
    u2 = TestRepoHelper.create_user!(%{groups: [g2.id, g3.id]})
    u3 = TestRepoHelper.create_user!(%{groups: [g3.id]})

    ds1 = TestRepoHelper.create_data_source!(%{groups: [g1.id]})
    ds2 = TestRepoHelper.create_data_source!(%{groups: [g2.id]})

    v1 = insert_view(ds1, u1, "view_1")
    v2 = insert_view(ds1, u1, "view_2")
    v3 = insert_view(ds2, u1, "view_3")
    v4 = insert_view(ds2, u2, "view_4")

    %{u1: u1, u2: u2, u3: u3, ds1: ds1, ds2: ds2, v1: v1, v2: v2, v3: v3, v4: v4}
  end

  test "fetching the changeset for the view", context, do:
    assert Ecto.Changeset.apply_changes(View.changeset(context.v1.id)) == context.v1

  test "fetching views for the user and the datasource", context do
    assert View.all(context.u1, context.ds1) |> Enum.sort_by(&(&1.id)) == [context.v1, context.v2]
    assert View.all(context.u1, context.ds2) == [context.v3]

    assert View.all(context.u2, context.ds1) == []
    assert View.all(context.u2, context.ds2) == [context.v4]

    assert View.all(context.u3, context.ds1) == []
    assert View.all(context.u3, context.ds2) == []
  end

  test "view requires name and sql", context do
    previous_views = View.all(context.u1, context.ds1) |> Enum.sort_by(&(&1.id))
    view = insert_view(context.ds1, context.u1, "view_5")
    assert :ok == View.delete(view.id, context.u1)
    assert View.all(context.u1, context.ds1) |> Enum.sort_by(&(&1.id)) == previous_views
  end

  test "cloak not available error", context do
    assert {:error, %Ecto.Changeset{errors: errors}} = View.create(context.u1, context.ds1, "name", "sql")
    assert {error, _} = Keyword.fetch!(errors, :sql)
    assert error ==
      "The view cannot be saved because no cloak is currently available for the given data source. " <>
      "Please contact your administrator."
  end

  test "deleting a view", context do
    assert {:error, %Ecto.Changeset{errors: errors}} = View.create(context.u1, context.ds1, "name", "sql")
    assert {error, _} = Keyword.fetch!(errors, :sql)
    assert error ==
      "The view cannot be saved because no cloak is currently available for the given data source. " <>
      "Please contact your administrator."
  end

  defp insert_view(data_source, user, name), do:
    %Air.Schemas.View{}
    |> Ecto.Changeset.cast(
      %{user_id: user.id, data_source_id: data_source.id, name: name, sql: "sql for #{name}", result_info: %{}},
      ~w(name sql user_id data_source_id result_info)a
    )
    |> Repo.insert!()
end
