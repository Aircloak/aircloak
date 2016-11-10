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

  test "running a query" do
    assert [
      {:send, row_description_message},
      {:send, data_row1_message},
      {:send, data_row2_message},
      {:send, command_complete_message},
      {:send, ready_for_query_message}
    ] =
      authenticate(true)
      |> run_actions(
            process: [query_message("select foo from bar")],
            select_result: [%{columns: [%{name: "x", type: :integer}], rows: [[1], [2]]}]
          )
      |> last_actions(5)

    assert backend_message_type(row_description_message) == :row_description
    assert backend_message_type(data_row1_message) == :data_row
    assert backend_message_type(data_row2_message) == :data_row
    assert ready_for_query_message == ready_for_query()
    assert command_complete_message == command_complete("SELECT 2")
  end


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

  defp last_action(protocol), do: hd(last_actions(protocol, 1))

  defp last_actions(protocol, n), do:
    protocol
    |> out_actions()
    |> Enum.reverse()
    |> Enum.take(n)
    |> Enum.reverse()
end
