defmodule AirWeb.Admin.DataSourceController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  import Aircloak.AssertionHelper
  alias Air.{TestSocketHelper, Schemas.DataSource, Repo}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})

    user = create_user!()
    admin = create_admin_user!()

    {:ok, user: user, admin: admin}
  end

  test "regular user can't manage data sources", context do
    register_data_source()
    conn = build_conn()

    assert login(context.user) |> get(admin_data_source_path(conn, :index)) |> redirected_to() === "/"
    assert login(context.user) |> get(admin_data_source_path(conn, :new)) |> redirected_to() === "/"

    assert soon(
             given_data_source(fn data_source ->
               login(context.user)
               |> get(admin_data_source_path(conn, :edit, data_source.name))
               |> redirected_to() === "/"
             end)
           )

    assert soon(
             given_data_source(fn data_source ->
               login(context.user)
               |> put(admin_data_source_path(conn, :update, data_source.name))
               |> redirected_to() === "/"
             end)
           )

    assert soon(
             given_data_source(fn data_source ->
               login(context.user)
               |> delete(admin_data_source_path(conn, :delete, data_source.name))
               |> redirected_to() === "/"
             end)
           )
  end

  test "lists data sources", context do
    register_data_source()

    assert soon(
             context.admin
             |> login()
             |> get(admin_data_source_path(build_conn(), :index))
             |> response(200) =~ "data_source_name"
           )
  end

  test "accessing edit", context do
    register_data_source()
    conn = build_conn()

    assert soon(
             given_data_source(fn data_source ->
               html =
                 login(context.admin)
                 |> get(admin_data_source_path(conn, :edit, data_source.name))
                 |> response(200)

               assert html =~ data_source.name
             end)
           )
  end

  test "deleting an unavailable data source", context do
    register_data_source()
    conn = build_conn()

    given_data_source(fn data_source ->
      assert context.admin
             |> login()
             |> delete(admin_data_source_path(conn, :delete, data_source.name))
             |> redirected_to() == admin_data_source_path(conn, :index)

      assert soon([] == Repo.all(DataSource))
    end)

    assert soon(Air.Repo.get_by(Air.Schemas.AuditLog, event: "Data source removal succeeded"))
  end

  test "render 404 on attempting to show non-existent data source", context do
    assert login(context.admin) |> get("/admin/data_sources/99999") |> response(404)
  end

  test "render 404 on attempting to render edit form for non-existent data source", context do
    assert login(context.admin) |> get("/admin/data_sources/99999/edit") |> response(404)
  end

  test "render 404 on attempting to update a non-existent data source", context do
    assert login(context.admin)
           |> put("/admin/data_sources/99999", data_source: %{name: "some name"})
           |> response(404)
  end

  test "render 404 on attempting to delete a non-existent data source", context do
    assert login(context.admin) |> delete("/admin/data_sources/99999") |> response(404)
  end

  defp register_data_source() do
    TestSocketHelper.with_cloak("cloak_name", "data_source_name", fn -> :ok end)
  end

  defp given_data_source(callback) do
    (fn ->
       case Repo.all(DataSource) do
         [data_source] -> callback.(data_source)
         [] -> false
       end
     end).()
  end
end
