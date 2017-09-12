defmodule Air.Admin.QueryControllerTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.Repo

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    params = %{
      "name" => "data source name",
      "tables" => "[]",
    }
    data_source = Air.Service.DataSource.create!(params)
    {:ok, data_source: data_source}
  end

  test "failed queries", context do
    user = create_user!()

    insert_query(user, context[:data_source], "query 1", %{error: "some error"})
    insert_query(user, context[:data_source], "query 2", %{error: "some error"})
    insert_query(user, context[:data_source], "query 3", %{})

    admin = create_admin_user!()
    response = login(admin) |> get("/admin/queries/failed") |> response(200)

    assert response =~ "query 1"
    assert response =~ "query 2"
    refute response =~ "query 3"
  end

  test "user can't fetch failed queries" do
    assert "/" ==
      create_user!()
      |> login()
      |> get("/admin/queries/failed")
      |> redirected_to()
  end

  defp insert_query(user, data_source, statement, result) do
    create_query!(user, %{
      statement: statement,
      data_source_id: data_source.id,
      result: result,
    })
  end
end
