defmodule AirWeb.Admin.DataSourceControllerTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.ConnCase, async: false

  import Air.TestConnHelper
  import Aircloak.AssertionHelper
  alias Air.{TestSocketHelper, TestRepoHelper, Schemas.DataSource, Repo}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  test "regular user can't manage data sources" do
    register_data_source()
    user = TestRepoHelper.create_user!()
    conn = build_conn()

    assert login(user) |> get(admin_data_source_path(conn, :index)) |> redirected_to() === "/"
    assert login(user) |> get(admin_data_source_path(conn, :new)) |> redirected_to() === "/"
    assert soon(given_data_source(fn(data_source) ->
      login(user) |> get(admin_data_source_path(conn, :edit, data_source.name)) |> redirected_to() === "/"
    end))
    assert soon(given_data_source(fn(data_source) ->
      login(user) |> put(admin_data_source_path(conn, :update, data_source.name)) |> redirected_to() === "/"
    end))
    assert soon(given_data_source(fn(data_source) ->
      login(user) |> delete(admin_data_source_path(conn, :delete, data_source.name)) |> redirected_to() === "/"
    end))
  end

  test "lists data sources" do
    register_data_source()
    admin = TestRepoHelper.create_admin_user!()

    assert soon(
      admin
      |> login()
      |> get(admin_data_source_path(build_conn(), :index))
      |> response(200) =~ "data_source_name"
    )
  end

  test "accessing edit" do
    register_data_source()
    admin = TestRepoHelper.create_admin_user!()
    conn = build_conn()

    assert soon(given_data_source(fn(data_source) ->
      html = login(admin) |> get(admin_data_source_path(conn, :edit, data_source.name)) |> response(200)
      assert html =~ data_source.name
    end))
  end

  test "deleting an unavailable data source" do
    register_data_source()
    admin = TestRepoHelper.create_admin_user!()
    conn = build_conn()

    assert soon(given_data_source(fn(data_source) ->
      assert admin
      |> login()
      |> delete(admin_data_source_path(conn, :delete, data_source.name))
      |> redirected_to() == admin_data_source_path(conn, :index)

      assert [] == Repo.all(DataSource)
    end))
  end

  test "render 404 on attempting to show non-existent data source" do
    admin = TestRepoHelper.create_admin_user!()
    assert login(admin) |> get("/admin/data_sources/99999") |> response(404)
  end

  test "render 404 on attempting to render edit form for non-existent data source" do
    admin = TestRepoHelper.create_admin_user!()
    assert login(admin) |> get("/admin/data_sources/99999/edit") |> response(404)
  end

  test "render 404 on attempting to update a non-existent data source" do
    admin = TestRepoHelper.create_admin_user!()
    assert login(admin) |> put("/admin/data_sources/99999", data_source: %{name: "some name"}) |> response(404)
  end

  test "render 404 on attempting to delete a non-existent data source" do
    admin = TestRepoHelper.create_admin_user!()
    assert login(admin) |> delete("/admin/data_sources/99999") |> response(404)
  end

  defp register_data_source() do
    TestSocketHelper.with_cloak("cloak_name", "data_source_name", fn -> :ok end)
  end

  defp given_data_source(callback) do
    (fn() ->
      case Repo.all(DataSource) do
        [data_source] -> callback.(data_source)
        [] -> false
      end
    end).()
  end
end
