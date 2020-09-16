defmodule AirWeb.Admin.CloaksController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.ConnCase, async: false

  import Air.TestConnHelper
  import Aircloak.AssertionHelper
  alias Air.Service.Cloak
  alias Air.TestRepoHelper

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  test "anonymous users cannot access the page", %{conn: conn} do
    conn |> get("/admin/cloaks") |> redirected_to()
  end

  test "non-admin users cannot access the page" do
    user = TestRepoHelper.create_user!()
    assert login(user) |> get("/admin/cloaks") |> redirected_to() === "/"
  end

  test "only shows cloaks while they are online" do
    admin = TestRepoHelper.create_admin_user!()
    {cloak_info, terminator} = connect_mock_cloak()
    assert login(admin) |> get("/admin/cloaks") |> response(200) =~ cloak_info.name
    terminator.()
    refute_soon login(admin) |> get("/admin/cloaks") |> response(200) =~ cloak_info.name
  end

  test "reinitializing cloak's data sources" do
    admin = TestRepoHelper.create_admin_user!()
    {cloak_info, terminator} = connect_mock_cloak()
    assert login(admin) |> post("/admin/cloaks/#{cloak_info.id}/reinitialize") |> redirected_to() == "/admin/cloaks"
    assert_receive {:cloak_message, {{AirWeb.Socket.Cloak.MainChannel, :cast}, "reinitialize_all_data_sources", nil}}
    terminator.()
  end

  @data_sources [%{name: "data_source_1", tables: []}]

  defp connect_mock_cloak() do
    cloak_info = TestRepoHelper.cloak_info("test_cloak_1")
    parent_pid = self()

    pid =
      spawn_link(fn ->
        Cloak.register(cloak_info, @data_sources)
        cloak_listener(parent_pid)
      end)

    {cloak_info, fn -> send(pid, :stop) end}
  end

  defp cloak_listener(parent_pid) do
    receive do
      :stop ->
        :ok

      message ->
        send(parent_pid, {:cloak_message, message})
        cloak_listener(parent_pid)
    end
  end
end
