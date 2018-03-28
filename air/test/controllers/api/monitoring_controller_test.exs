defmodule AirWeb.API.MonitoringController.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use AirWeb.MonitoringEndpointCase, async: false

  import Air.TestRepoHelper

  test "error when not authenticated", %{conn: conn} do
    body = get(conn, "/") |> response(401) |> Poison.decode!()
    refute body["success"]
    assert body["description"] =~ ~r/authenticate/
  end

  test "returns monitoring blog as JSON", %{conn: conn} do
    token = create_monitoring_token!()

    body =
      conn
      |> get("/?auth_token=#{token}")
      |> response(200)
      |> Poison.decode!()

    for expected_key <- ~w(cloaks data_sources groups users version), do: assert(Map.has_key?(body, expected_key))
  end
end
