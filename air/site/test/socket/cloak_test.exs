defmodule Air.Socket.CloakTest do
  use ExUnit.Case, async: true

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket


  test "invalid authentication" do
    assert {:ok, socket} = start_link(url(%{}))
    assert {:disconnected, {403, "Forbidden"}} == TestSocket.wait_connect_status(socket)
  end

  test "unmatched topic" do
    assert {:ok, socket} = start_link(url())
    assert :connected == TestSocket.wait_connect_status(socket)
    assert {:error, reason} = TestSocket.join(socket, "invalid_channel")
    assert {:server_rejected, "invalid_channel", %{"reason" => "unmatched topic"}} == reason
  end

  test "main topic" do
    assert {:ok, socket} = start_link(url())
    assert :connected == TestSocket.wait_connect_status(socket)
    assert {:ok, {"main", %{}}} == TestSocket.join(socket, "main")
  end

  defp start_link(url) do
    TestSocket.start_link(GenSocketClient.Transport.WebSocketClient, url, true,
        serializer: GenSocketClient.Serializer.GzipJson)
  end

  defp url(params \\ %{
        cloak_name: "cloak_token_1"
      }) do
    "#{Air.Endpoint.url}/cloak/socket/websocket?#{URI.encode_query(params)}"
    |> String.replace(~r(http://), "ws://")
    |> String.replace(~r(https://), "wss://")
  end
end
