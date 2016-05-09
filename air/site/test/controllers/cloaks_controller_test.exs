defmodule Air.CloaksControllerTest do
  use Air.ConnCase

  import Air.TestConnHelper
  alias Air.{TestRepoHelper, TestSocketHelper}
  alias Phoenix.Channels.GenSocketClient.TestSocket

  test "anonymous user can't access the page", %{conn: conn} do
    conn |> get("/cloaks") |> redirected_to()
  end

  test "listing connected cloaks" do
    user =
    TestRepoHelper.create_organisation!("unknown_org")
    |> TestRepoHelper.create_user!(:user)

    # connect a mock cloak
    socket = TestSocketHelper.connect!(%{cloak_name: "test_cloak1"})
    TestSocketHelper.join!(socket, "main", %{name: "test_cloak1", data_sources: []})

    # verify that it's in the list
    html_response = login(user) |> get("/cloaks") |> response(200)
    assert html_response =~ "test_cloak1"

    # disconnect the cloak
    mref = Process.monitor(Air.CloakInfo.main_channel_pid("unknown_org/test_cloak1"))
    TestSocket.leave(socket, "main")
    assert_receive {:DOWN, ^mref, _, _, _}

    # it shouldn't be in the list anymore
    html_response = login(user) |> get("/cloaks") |> response(200)
    refute html_response =~ "test_cloak1"
  end

  test "cloaks from other orgs are not listed" do
    user =
    TestRepoHelper.create_organisation!("another_org")
    |> TestRepoHelper.create_user!(:user)

    socket = TestSocketHelper.connect!(%{cloak_name: "test_cloak2"})
    TestSocketHelper.join!(socket, "main", %{name: "test_cloak2", data_sources: []})
    html_response = login(user) |> get("/cloaks") |> response(200)
    refute html_response =~ "test_cloak2"
  end
end
