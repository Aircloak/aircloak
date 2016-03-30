defmodule Air.Socket.CloakTest do
  use ExUnit.Case, async: true

  alias Channels.Client.SocketDriver

  test "invalid authentication" do
    assert {:ok, socket} = start_link(url(%{shared_secret: "invalid shared secret"}))
    assert {:error, {403, "Forbidden"}} == SocketDriver.connect(socket)
  end

  test "unmatched topic" do
    assert {:ok, socket} = start_link(url())
    assert :ok == SocketDriver.connect(socket)
    assert {:error, reason} = SocketDriver.join(socket, "invalid_channel")
    assert {:server_rejected, "invalid_channel", %{"reason" => "unmatched topic"}} == reason
  end

  test "main topic" do
    assert {:ok, socket} = start_link(url())
    assert :ok == SocketDriver.connect(socket)
    assert {:ok, {"main", %{}}} == SocketDriver.join(socket, "main")
  end

  defp start_link(url) do
    SocketDriver.start_link(url, serializer: Channels.Client.Socket.Serializer.GzipJson)
  end

  defp url(params \\ %{
        shared_secret: :air_etcd.get("/service/airpub/shared_secret"),
        cloak_token: "cloak_token_1"
      }) do
    "#{Air.Endpoint.url}/cloak/socket/websocket?#{URI.encode_query(params)}"
    |> String.replace(~r(http://), "ws://")
    |> String.replace(~r(https://), "wss://")
  end
end
