defmodule Air.PsqlServer.ProtocolTest do
  use ExUnit.Case, async: true

  alias Air.PsqlServer.Protocol
  import Air.PsqlServer.Protocol.Messages

  test "invalid version number" do
    major = 4
    minor = 0
    assert {:close, :unsupported_protocol_version} ==
      negotiate_ssl()
      |> run_actions(process: [<<8::32, major::16, minor::16>>])
      |> last_action()
  end

  test "normal login sequence", do:
    assert {:send, ready_for_query()} == last_action(authenticate(true))

  test "failed login", do:
    assert {:close, :not_authenticated} == last_action(authenticate(false))

  test "ssl required" do
    assert {:close, :required_ssl} ==
      Protocol.new()
      |> run_actions(process: [<<23::32, 3::16, 0::16, "user", 0, "some_user", 0>>])
      |> last_action()
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp negotiate_ssl() do
    protocol = run_actions(Protocol.new(),
      process: [<<8::32, 1234::16, 5679::16>>],
      ssl_negotiated: []
    )

    {actions, protocol} = Protocol.actions(protocol)
    assert actions == [{:send, require_ssl()}, :upgrade_to_ssl]

    protocol
  end

  defp authenticate(authentication_success) do
    protocol =
      run_actions(negotiate_ssl(),
        process: [<<23::32, 3::16, 0::16, "user", 0, "some_user", 0>>],
        authentication_method: [:cleartext],
        process: [<<?p, 18::32, "some_password", 0>>]
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
