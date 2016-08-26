defmodule Air.CloaksControllerTest do
  use Air.ConnCase

  import Air.{TestConnHelper, AssertionHelper}
  alias Air.{TestRepoHelper, DataSourceManager}

  test "anonymous user can't access the page", %{conn: conn} do
    conn |> get("/cloaks") |> redirected_to()
  end

  test "only shows cloaks while they are online" do
    org = TestRepoHelper.create_organisation!("unknown_org")
    user = TestRepoHelper.create_user!(org, :user)

    # connect a mock cloak
    {terminator, pid} = temporarily_alive()
    cloak_info = %{
      channel_pid: pid,
      id: "cloak id",
      name: "cloak name",
      online_since: Timex.DateTime.now(),
    }

    data_sources = [%{"id" => "id", "name" => "name", "tables" => []}]
    DataSourceManager.register_cloak(cloak_info, data_sources)

    # verify that it's in the list
    html_response = login(user) |> get("/cloaks") |> response(200)
    assert html_response =~ "cloak name"

    # disconnect the cloak
    terminator.()

    # verify that it's in the list
    refute soon((login(user) |> get("/cloaks") |> response(200)) =~ "test_cloak1")
  end

  defp temporarily_alive() do
    pid = spawn(fn -> receive do :stop -> :ok end end)
    {fn -> send(pid, :stop) end, pid}
  end
end
