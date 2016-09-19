defmodule Air.Admin.DataSourceControllerTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, AssertionHelper}
  alias Air.{TestSocketHelper, TestRepoHelper, DataSource, Repo}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "regular user can't manage data sources" do
    register_data_source()

    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :user)

    conn = build_conn()

    assert login(user) |> get(admin_data_source_path(conn, :index)) |> redirected_to() === "/"
    assert login(user) |> get(admin_data_source_path(conn, :new)) |> redirected_to() === "/"
    assert soon(given_data_source(fn(data_source) ->
      login(user) |> get(admin_data_source_path(conn, :edit, data_source.id)) |> redirected_to() === "/"
    end))
    assert soon(given_data_source(fn(data_source) ->
      login(user) |> put(admin_data_source_path(conn, :update, data_source.id)) |> redirected_to() === "/"
    end))
    assert soon(given_data_source(fn(data_source) ->
      login(user) |> delete(admin_data_source_path(conn, :delete, data_source.id)) |> redirected_to() === "/"
    end))
  end

  test "lists data sources" do
    register_data_source()

    organisation = TestRepoHelper.admin_organisation()
    user = TestRepoHelper.create_user!(organisation)

    assert soon(
      user
      |> login()
      |> get(admin_data_source_path(build_conn(), :index))
      |> response(200) =~ "data_source_name"
    )
  end

  test "accessing edit" do
    register_data_source()

    organisation = TestRepoHelper.admin_organisation()
    user = TestRepoHelper.create_user!(organisation)

    conn = build_conn()

    assert soon(given_data_source(fn(data_source) ->
      html = login(user) |> get(admin_data_source_path(conn, :edit, data_source.id)) |> response(200)
      assert html =~ data_source.name
    end))
  end

  test "can edit data source name" do
    register_data_source()

    organisation = TestRepoHelper.admin_organisation()
    user = TestRepoHelper.create_user!(organisation)

    conn = build_conn()

    assert soon(given_data_source(fn(data_source) ->
      assert user
      |> login()
      |> put(admin_data_source_path(conn, :update, data_source.id), data_source: %{name: "new name"})
      |> redirected_to() == admin_data_source_path(conn, :index)

      data_source = Repo.get(DataSource, data_source.id)
      assert data_source.name == "new name"
    end))
  end

  test "deleting an unavailable data source" do
    register_data_source()

    organisation = TestRepoHelper.admin_organisation()
    user = TestRepoHelper.create_user!(organisation)

    conn = build_conn()

    assert soon(given_data_source(fn(data_source) ->
      assert user
      |> login()
      |> delete(admin_data_source_path(conn, :delete, data_source.id))
      |> redirected_to() == admin_data_source_path(conn, :index)

      assert [] == Repo.all(DataSource)
    end))
  end

  defp register_data_source() do
    organisation = TestRepoHelper.admin_organisation()
    TestSocketHelper.with_cloak("cloak_name", organisation.name, "data_source_name", fn -> :ok end)
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
