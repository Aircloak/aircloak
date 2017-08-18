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
  def handle_client_message(%{state: :login_params} = protocol, :raw, raw_login_params), do:
    protocol
    |> Protocol.add_action({:login_params, Messages.decode_login_params(raw_login_params)})
    |> Protocol.next_state(:authenticating)
  def handle_client_message(protocol, :password, password), do:
    protocol
    |> Protocol.add_action({:authenticate, password})
    |> Protocol.next_state(:authenticating)

  @doc false
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
end
