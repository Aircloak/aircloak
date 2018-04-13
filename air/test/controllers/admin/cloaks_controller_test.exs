defmodule AirWeb.Admin.CloaksController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.ConnCase, async: false

  import Air.TestConnHelper
  import Aircloak.AssertionHelper
  alias Air.Service.Cloak
  import Air.TestRepoHelper

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  test "anonymous user can't access the page", %{conn: conn} do
    conn |> get("/admin/cloaks") |> redirected_to()
  end

  test "only shows cloaks while they are online" do
    admin = create_admin_user!()
    create_privacy_policy_and_accept_it!(admin)

    # connect a mock cloak
    {terminator, pid} = temporary_process()

    cloak_info = %{
      channel_pid: pid,
      id: "cloak id",
      name: "cloak name",
      online_since: Timex.now()
    }

    data_sources = [%{name: "cloak_name", tables: []}]
    Cloak.register(cloak_info, data_sources)

    # verify that it's in the list
    html_response = login(admin) |> get("/admin/cloaks") |> response(200)
    assert html_response =~ "cloak name"

    # disconnect the cloak
    terminator.()

    # verify that it's in the list
    refute soon(login(admin) |> get("/admin/cloaks") |> response(200) =~ "test_cloak1")
  end

  test "only shows cloaks to admin" do
    user = create_user!()
    create_privacy_policy_and_accept_it!(user)

    admin = create_admin_user!()
    accept_privacy_policy!(admin)

    # connect a mock cloak
    {terminator, pid} = temporary_process()

    cloak_info = %{
      channel_pid: pid,
      id: "cloak id",
      name: "cloak name",
      online_since: Timex.now()
    }

    data_sources = [%{name: "cloak_name", tables: []}]
    Cloak.register(cloak_info, data_sources)

    # verify that it's in the list
    assert login(user) |> get("/admin/cloaks") |> redirected_to() === "/"
    html_response = login(admin) |> get("/admin/cloaks") |> response(200)
    assert html_response =~ "cloak name"

    # disconnect the cloak
    terminator.()

    # verify that it's in the list
    refute soon(login(admin) |> get("/admin/cloaks") |> response(200) =~ "test_cloak1")
  end

  defp temporary_process() do
    pid =
      spawn_link(fn ->
        receive do
          :stop -> :ok
        end
      end)

    {fn -> send(pid, :stop) end, pid}
  end
end
