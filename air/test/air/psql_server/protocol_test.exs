defmodule Air.PsqlServer.ProtocolTest do
  use ExUnit.Case, async: true

  alias Air.PsqlServer.Protocol
  import Air.PsqlServer.Protocol.Messages

  test "invalid version number" do
    assert {:close, :unsupported_protocol_version} ==
      negotiate_ssl()
      |> run_actions(process: [startup_message(4, 0, [])])
      |> last_action()
  end

  test "normal login sequence", do:
    assert {:send, ready_for_query()} == last_action(authenticate(true))

  test "failed login", do:
    assert {:close, :not_authenticated} == last_action(authenticate(false))

  test "ssl required" do
    assert {:close, :required_ssl} ==
      Protocol.new()
      |> run_actions(process: [startup_message(3, 0, [])])
      |> last_action()
  end

  test "termination", do:
    assert {:close, :normal} ==
      authenticate(true)
      |> run_actions(process: [terminate_message()])
      |> last_action()


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp negotiate_ssl() do
    protocol = run_actions(Protocol.new(),
      process: [ssl_message()],
      ssl_negotiated: []
    )

    {actions, protocol} = Protocol.actions(protocol)
    assert actions == [{:send, require_ssl()}, :upgrade_to_ssl]

    protocol
  end

  defp authenticate(authentication_success) do
    protocol =
      run_actions(negotiate_ssl(),
        process: [startup_message(3, 0, user: "some_user")],
        authentication_method: [:cleartext],
        process: [password_message("some_password")]
      )

    {actions, protocol} = Protocol.actions(protocol)
    assert actions == [
      login_params: %{"user" => "some_user"},
      send: authentication_method(:cleartext),
      authenticate: "some_password"
    ]

    run_actions(protocol, authenticated: [authentication_success])
  end

  defp run_actions(protocol, actions) do
    Enum.reduce(
      actions,
      protocol,
      fn({fun_name, args}, protocol_acc) -> apply(Protocol, fun_name, [protocol_acc | args]) end
    )
  end

  defp out_actions(protocol) do
    {actions, _} = Protocol.actions(protocol)
    actions
  end

  defp last_action(protocol), do: List.last(out_actions(protocol))
end
