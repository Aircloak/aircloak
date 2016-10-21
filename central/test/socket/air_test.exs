defmodule Central.Socket.AirTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use ExUnit.Case, async: false

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Central.{TestSocketHelper, Repo}
  alias Central.Service.Customer

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "air name must be provided" do
    params = %{air_name: ""}
    assert {{:disconnected, {403, "Forbidden"}}, _} = TestSocketHelper.connect(params)
  end

  test "unmatched topic" do
    socket = connect!()
    assert {:error, reason} = TestSocket.join(socket, "invalid_channel")
    assert {:server_rejected, "invalid_channel", %{"reason" => "unmatched topic"}} == reason
  end

  test "main topic" do
    socket = connect!()
    assert {:ok, %{}} == join_main_channel(socket)
  end

  defp connect!(params \\ %{}) do
    TestSocketHelper.connect!(Map.merge(default_params, params))
  end

  defp join_main_channel(socket) do
    TestSocketHelper.join!(socket, "main", %{})
  end

  defp default_params() do
    {:ok, customer} = Customer.create(%{name: "test customer"})
    {:ok, token} = Customer.generate_token(customer)
    %{air_name: "air_1", token: token}
  end
end
