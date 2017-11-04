defmodule AirWeb.Socket.CloakTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use ExUnit.Case, async: false

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Air.{TestSocketHelper, Repo}

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "cloak name must be provided" do
    params = %{cloak_name: ""}
    assert {{:disconnected, {403, "Forbidden"}}, _} = TestSocketHelper.connect(params)
  end

  test "unmatched topic" do
    import ExUnit.CaptureLog

    capture_log(fn ->
      socket = connect!()
      assert {:error, reason} = TestSocket.join(socket, "invalid_channel")
      assert {:server_rejected, "invalid_channel", %{reason: "unmatched topic"}} == reason
    end)
  end

  defp connect!(params \\ %{}) do
    default_params = %{cloak_name: "cloak_1"}
    TestSocketHelper.connect!(Map.merge(default_params, params))
  end
end
