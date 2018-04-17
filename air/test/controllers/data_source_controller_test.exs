defmodule Air.DataSourceControllerTest do
  use AirWeb.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper

  test "can see data sources assigned to a group the user belongs to", %{conn: conn} do
    group = TestRepoHelper.create_group!()
    data_source = TestRepoHelper.create_data_source!(%{groups: [group.id]})
    user = TestRepoHelper.create_user!(%{groups: [group.id]})

    assert login(user) |> get(data_source_path(conn, :index)) |> response(200) =~ data_source.name
  end

  test "can't see data sources assigned to a group the user does not belong to", %{conn: conn} do
    group1 = TestRepoHelper.create_group!()
    group2 = TestRepoHelper.create_group!()
    data_source = TestRepoHelper.create_data_source!(%{groups: [group1.id]})
    user = TestRepoHelper.create_user!(%{groups: [group2.id]})

    refute login(user) |> get(data_source_path(conn, :index)) |> response(200) =~ data_source.name
  end
end
