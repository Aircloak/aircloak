defmodule AirWeb.Admin.QueryController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.Repo

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})

    {:ok, user: create_user!()}
  end

  test "failed queries", %{user: user} do
    admin = create_admin_user!()
    data_source = Air.Service.DataSource.create!(%{"name" => "data source", "tables" => "[]"})
    ignored_data_source = Air.Service.DataSource.create!(%{"name" => "ignored data source", "tables" => "[]"})
    ignored_user = create_user!()

    insert_query(user, data_source, "query 1", :error)
    insert_query(user, data_source, "query 2", :error)
    insert_query(user, data_source, "query 3", :completed)
    insert_query(user, ignored_data_source, "query 4", :error)
    insert_query(ignored_user, data_source, "query 5", :error)

    response =
      login(admin) |> get("/admin/queries/failed", %{users: [user.id], data_sources: [data_source.id]}) |> response(200)

    assert response =~ "query 1"
    assert response =~ "query 2"
    refute response =~ "query 3"
    refute response =~ "query 4"
    refute response =~ "query 5"
  end

  test "user can't fetch failed queries", %{user: user} do
    assert(
      "/" ==
        user
        |> login()
        |> get("/admin/queries/failed")
        |> redirected_to()
    )
  end

  defp insert_query(user, data_source, statement, query_state) do
    create_query!(user, %{
      statement: statement,
      data_source_id: data_source.id,
      query_state: query_state
    })
  end
end
