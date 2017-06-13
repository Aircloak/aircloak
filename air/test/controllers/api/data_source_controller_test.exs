defmodule Air.API.DataSourceController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{TestSocketHelper, Schemas.DataSource, Repo}
  alias Poison, as: JSON

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "getting all sources" do
    user = create_user!()
    api_token = create_token!(user)
    cloak_name = "cloak_name"

    [api_cloak] = TestSocketHelper.with_cloak(cloak_name, "data_source_name", fn ->
      api_conn(api_token)
      |> get("/api/data_sources")
      |> response(200)
      |> JSON.decode!()
    end)

    [data_source] = Repo.all(DataSource)
    assert api_cloak["name"] == "data_source_name"
    assert api_cloak["tables"] == []
    assert api_cloak["id"] == data_source.id
  end

  test "includes views amongst talbes" do
    group = create_group!()
    user = create_user!(%{groups: [group.id]})
    api_token = create_token!(user)
    cloak_name = "cloak_name"
    view_name = "view1"

    [api_cloak] = TestSocketHelper.with_cloak(cloak_name, "data_source_name", fn ->
      create_view(user, view_name)

      api_conn(api_token)
      |> get("/api/data_sources")
      |> response(200)
      |> JSON.decode!()
    end)

    assert [%{"id" => ^view_name}] = api_cloak["tables"]
  end

  defp create_view(user, view_name) do
    [data_source] = Repo.all(DataSource)
    %Air.Schemas.View{}
    |> Ecto.Changeset.cast(
      %{
        user_id: user.id,
        data_source_id: data_source.id,
        name: view_name,
        sql: "sql for #{view_name}",
        result_info: %{"columns" => ["foo", "bar"]},
      },
      ~w(name sql user_id data_source_id result_info)a
    )
    |> Repo.insert!()
  end
end
