defmodule Air.PsqlServer.Protocol.ConnectionSetup do
  @moduledoc """
  Handles the initial connection setup part of PostgreSQL protocol.

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
    handle_intent_message(protocol, message)
  def handle_client_message(%{state: :cancelling_query} = protocol, :raw, message), do:
    protocol
    |> Protocol.add_action({:cancel_query, Messages.decode_cancel_message_parameters(message)})
    |> Protocol.close(:normal)

  @doc false
  def handle_event(%{state: :negotiating_ssl} = protocol, :ssl_negotiated), do:
    Protocol.await_client_message(protocol, state: :ssl_negotiated, bytes: 8, decode?: false)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp handle_intent_message(protocol, message) do
    if Messages.cancel_message?(message) do
      Protocol.await_client_message(protocol, state: :cancelling_query, bytes: 8, decode?: false)

    else
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
  end

  defp handle_initial_message(protocol, message, %{msg_type: :startup, ssl_required: false}), do:
    # TCP connection and SSL is not required -> proceed
    handle_intent_message(protocol, message)
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
