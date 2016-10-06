defmodule Air.PsqlServer.ProtocolTest do
  use ExUnit.Case, async: true

  alias Air.PsqlServer.Protocol
  import Air.PsqlServer.Protocol.Messages

  test "invalid version number" do
    out_actions =
      Protocol.new()
      |> run_actions(process: ["12345678"])
      |> out_actions()

    assert out_actions == [close: :unsupported_protocol_version]
  end

  test "normal login sequence" do
    out_actions =
      Protocol.new()
      |> standard_login()
      |> out_actions()

    assert out_actions == [
      login_params: %{"user" => "some_user"},
      send: authentication_method(:cleartext),
      authenticate: "some_password",
      send: authentication_ok(),
      send: ready_for_query()
    ]
  end

  test "login with ssl attempt" do
    out_actions =
      Protocol.new()
      |> run_actions(process: [<<8::32, 1234::16, 5679::16>>])
      |> standard_login()
      |> out_actions()

    assert out_actions == [
      send: no_ssl(),
      login_params: %{"user" => "some_user"},
      send: authentication_method(:cleartext),
      authenticate: "some_password",
      send: authentication_ok(),
      send: ready_for_query()
    ]
  end

  defp standard_login(protocol) do
    run_actions(protocol, [
      process: [<<23::32, 3::16, 0::16, "user", 0, "some_user", 0>>],
      authentication_method: [:cleartext],
      process: [<<?p, 18::32, "some_password", 0>>],
      authenticated: []
    ])
  end

  defp out_actions(protocol) do
    {actions, _} = Protocol.actions(protocol)
    actions
  end

  defp run_actions(protocol, actions) do
    Enum.reduce(
      actions,
      protocol,
      fn({fun_name, args}, protocol_acc) -> apply(Protocol, fun_name, [protocol_acc | args]) end
    )
  end
end
