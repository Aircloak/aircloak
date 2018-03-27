defmodule AirWeb.API.DataSourceController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.ConnCase, async: false

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

    [api_cloak] =
      TestSocketHelper.with_cloak(cloak_name, "data_source_name", fn ->
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

    [api_cloak] =
      TestSocketHelper.with_cloak(cloak_name, "data_source_name", fn ->
        create_view!(user, get_datasource(), view_name)

        api_conn(api_token)
        |> get("/api/data_sources")
        |> response(200)
        |> JSON.decode!()
      end)

    assert [%{"id" => ^view_name}] = api_cloak["tables"]
  end

  defp get_datasource(), do: hd(Repo.all(DataSource))
end
