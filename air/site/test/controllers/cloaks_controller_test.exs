defmodule Air.CloaksControllerTest do
  use Air.ConnCase

  import Air.TestConnHelper
  alias Air.{TestRepoHelper, TestSocketHelper}
  alias Phoenix.Channels.GenSocketClient.TestSocket

  test "anonymous user can't access the page", %{conn: conn} do
    conn |> get("/cloaks") |> redirected_to()
  end

  test "listing connected cloaks" do
    org = TestRepoHelper.create_organisation!("unknown_org")
    user = TestRepoHelper.create_user!(org, :user)

    # connect a mock cloak
    socket = TestSocketHelper.connect!(%{cloak_name: "test_cloak1", cloak_organisation: org.name})
    TestSocketHelper.join!(socket, "main", %{data_sources: []})

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
    organisation = TestRepoHelper.create_organisation!("user_org")
    user = TestRepoHelper.create_user!(organisation, :user)

    socket = TestSocketHelper.connect!(%{cloak_name: "test_cloak2", cloak_organisation: "another_org"})
    TestSocketHelper.join!(socket, "main", %{data_sources: []})
    html_response = login(user) |> get("/cloaks") |> response(200)
    refute html_response =~ "test_cloak2"
  end
end
