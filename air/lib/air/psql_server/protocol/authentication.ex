defmodule Air.PsqlServer.Protocol.Authentication do
  @moduledoc """
  Handles the authentication part of PostgreSQL protocol.

  For more details see [here](https://www.postgresql.org/docs/9.6/static/protocol-flow.html#AEN112777).
  """

  alias Air.PsqlServer.Protocol
  alias Air.PsqlServer.Protocol.Messages
  require Aircloak.DeployConfig
  require Logger

  @behaviour Protocol


  # -------------------------------------------------------------------
  # Protocol callbacks
  # -------------------------------------------------------------------

  @doc false
  def handle_client_message(%{state: :initial} = protocol, :raw, message), do:
    handle_initial_message(protocol, message,
      %{
        msg_type: (if Messages.ssl_message?(message), do: :ssl, else: :startup),
        ssl_valid: Air.PsqlServer.validate_ssl_config(),
        ssl_required: Air.PsqlServer.configuration().require_ssl
      }
    )
  def handle_client_message(%{state: :ssl_negotiated} = protocol, :raw, message), do:
    handle_startup_message(protocol, message)
  def handle_client_message(%{state: :login_params} = protocol, :raw, raw_login_params), do:
    protocol
    |> Protocol.add_action({:login_params, Messages.decode_login_params(raw_login_params)})
    |> Protocol.next_state(:authenticating)
  def handle_client_message(protocol, :password, password), do:
    protocol
    |> Protocol.add_action({:authenticate, password})
    |> Protocol.next_state(:authenticating)

  @doc false
  def handle_event(%{state: :negotiating_ssl} = protocol, :ssl_negotiated), do:
    Protocol.await_client_message(protocol, state: :ssl_negotiated, bytes: 8, decode?: false)
  def handle_event(%{state: :authenticating} = protocol, {:authentication_method, method}), do:
    protocol
    |> Protocol.send_to_client({:authentication_method, method})
    |> Protocol.await_client_message()
  def handle_event(%{state: :authenticating} = protocol, {:authenticated, true}) do
    protocol
    |> Protocol.send_to_client(:authentication_ok)
    |> Protocol.send_to_client({:parameter_status, "application_name", "aircloak"})
    |> Protocol.send_to_client({:parameter_status, "server_version", "1.0.0"})
    |> Protocol.send_to_client(:ready_for_query)
    |> Protocol.await_client_message(state: :ready)
  end
  def handle_event(%{state: :authenticating} = protocol, {:authenticated, false}), do:
    protocol
    # We're sending AuthenticationOK to indicate to the client that the auth procedure went fine. Then
    # we'll send a fatal error with a custom error message. It is unclear from the official docs that it
    # should be done this way. However, this approach produces a nicer error message, and it's the same
    # in PostgreSQL server (determined by wireshark).
    |> Protocol.send_to_client(:authentication_ok)
    |> Protocol.send_to_client({:fatal_error, "Authentication failed!"})
    |> Protocol.close(:not_authenticated)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp handle_startup_message(protocol, message) do
    startup_message = Messages.decode_startup_message(message)
    if startup_message.version.major != 3 do
      Protocol.close(protocol, :unsupported_protocol_version)
    else
      Protocol.await_client_message(protocol,
        state: :login_params,
        bytes: startup_message.length,
        decode?: false
      )
    end
  end

  defp handle_initial_message(protocol, message, %{msg_type: :startup, ssl_required: false}), do:
    # TCP connection and SSL is not required -> proceed
    handle_startup_message(protocol, message)
  defp handle_initial_message(protocol, _message, %{msg_type: :ssl, ssl_valid: :ok}), do:
    # SSL connection and SSL is valid -> proceed
    protocol
    |> Protocol.send_to_client(:require_ssl)
    |> Protocol.add_action(:upgrade_to_ssl)
    |> Protocol.next_state(:negotiating_ssl)
  defp handle_initial_message(protocol, _message, %{msg_type: :ssl, ssl_valid: {:error, _}, ssl_required: false}), do:
    # SSL connection and SSL is not valid but also not required -> inform client that we don't support SSL and allow it
    # to retry with unencrypted TCP
    protocol
    |> Protocol.send_to_client(:ssl_not_supported)
    |> Protocol.await_client_message(state: :initial, bytes: 8, decode?: false)
  defp handle_initial_message(protocol, _message, %{msg_type: :startup, ssl_required: true}), do:
    # TCP connection and SSL is required -> we can't proceed so we're closing the connection
    protocol
    |> Protocol.send_to_client({:fatal_error, "Only SSL connections are allowed!"})
    |> Protocol.close(:tcp_forbidden)
  defp handle_initial_message(protocol, _message, %{msg_type: :ssl, ssl_valid: {:error, error}, ssl_required: true}) do
    # SSL connection and SSL is required but not valid -> we can't proceed so we're closing the connection
    Logger.error(error)

    protocol
    |> Protocol.send_to_client({:fatal_error, "SSL is not properly configured on the server!"})
    |> Protocol.close(:ssl_not_configured)
  end
end
