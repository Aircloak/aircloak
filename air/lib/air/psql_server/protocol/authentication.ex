defmodule Air.PsqlServer.Protocol.Authentication do
  @moduledoc false

  # Authentication part of PostgreSQL protocol. Handles ssl negotiation, and initial authentication.

  import Air.PsqlServer.Protocol.Helpers
  alias Air.PsqlServer.Protocol.Messages

  def handle_client_message(%{state: :initial} = protocol, :raw, message) do
    if Messages.ssl_message?(message) do
      protocol
      |> send_to_client(:require_ssl)
      |> add_action(:upgrade_to_ssl)
      |> next_state(:negotiating_ssl)
    else
      protocol
      |> send_to_client(:fatal_error_message, ["Only SSL connections are allowed!"])
      |> close(:required_ssl)
    end
  end
  def handle_client_message(%{state: :ssl_negotiated} = protocol, :raw, message) do
    startup_message = Messages.decode_startup_message(message)
    if startup_message.version.major != 3 do
      close(protocol, :unsupported_protocol_version)
    else
      next_state(protocol, :login_params, startup_message.length)
    end
  end
  def handle_client_message(%{state: :login_params} = protocol, :raw, raw_login_params), do:
    protocol
    |> add_action({:login_params, Messages.decode_login_params(raw_login_params)})
    |> next_state(:choosing_authentication_method)
  def handle_client_message(protocol, :password, password), do:
    protocol
    |> add_action({:authenticate, password})
    |> next_state(:authenticating)

  def ssl_negotiated(%{state: :negotiating_ssl} = protocol), do:
    next_state(protocol, :ssl_negotiated, 8)

  def authentication_method(%{state: :choosing_authentication_method} = protocol, authentication_method), do:
    protocol
    |> send_to_client(:authentication_method, [authentication_method])
    |> transition_after_message(:awaiting_password)

  def authenticated(%{state: :authenticating} = protocol, true) do
    protocol
    |> send_to_client(:authentication_ok)
    |> send_to_client(:parameter_status, ["application_name", "aircloak"])
    |> send_to_client(:parameter_status, ["server_version", "1.0.0"])
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:ready)
  end
  def authenticated(%{state: :authenticating} = protocol, false), do:
    protocol
    # We're sending AuthenticationOK to indicate to the client that the auth procedure went fine. Then
    # we'll send a fatal error with a custom error message. It is unclear from the official docs that it
    # should be done this way. However, this approach produces a nicer error message, and it's the same
    # in PostgreSQL server (determined by wireshark).
    |> send_to_client(:authentication_ok)
    |> send_to_client(:fatal_error_message, ["Authentication failed!"])
    |> close(:not_authenticated)
end
