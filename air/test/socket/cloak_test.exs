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

  describe "when cloak_secret not configured" do
    test "connects normally" do
      assert {:ok, _} = connect!() |> TestSocket.join("main", cloak_data())
    end
  end

  describe "when cloak_secret configured" do
    setup do
      require Aircloak.DeployConfig
      site_settings = Aircloak.DeployConfig.fetch!("site")
      Aircloak.DeployConfig.update("site", fn _ -> Map.put(site_settings, "cloak_secret", "some secret") end)
      on_exit(fn -> Aircloak.DeployConfig.update("site", fn _ -> site_settings end) end)
    end

    test "rejects connections without valid proof" do
      assert {:error, {:server_rejected, "main", :cloak_secret_invalid}} =
               connect!() |> TestSocket.join("main", cloak_data())
    end

    test "allows connections with valid proof" do
      proof = Aircloak.SharedSecret.proof("some secret")
      assert {:ok, _} = connect!() |> TestSocket.join("main", cloak_data(%{secret_proof: proof}))
    end
  end

  defp cloak_data(overrides \\ %{}) do
    Map.merge(%{salt_hash: "some salt hash", data_sources: []}, overrides)
  end

  defp connect!(params \\ %{}) do
    default_params = %{cloak_name: "cloak_1"}
    TestSocketHelper.connect!(Map.merge(default_params, params))
  end
end
