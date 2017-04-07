defmodule Air.PsqlServer.Protocol.Helpers do
  @moduledoc false

  # Helper functions for working with protocol state, such as dispatching client messages, moving to the next
  # state, awaiting for a message, ...

  require Logger
  alias Air.PsqlServer.Protocol.Messages

  def actions(protocol), do:
    {Enum.reverse(protocol.actions), %{protocol | actions: []}}

  def process_buffer(%{expecting: expecting, buffer: buffer} = protocol)
      when expecting > 0 and byte_size(buffer) >= expecting do
    <<message::binary-size(expecting)>> <> rest_buffer = buffer

    %{protocol | expecting: 0, buffer: rest_buffer}
    |> dispatch_message(message)
    |> process_buffer()
  end
  def process_buffer(protocol), do: protocol

  def send_to_client(protocol, message, args \\ []) do
    debug_log(protocol, fn ->
      ["psql server: sending ", to_string(message), " ", inspect(args)]
    end)
    add_action(protocol, {:send, apply(Messages, message, args)})
  end

  def add_action(protocol, action), do: %{protocol | actions: [action | protocol.actions]}

  def next_state(protocol, next_state_name, expecting \\ 0), do:
    %{protocol | name: next_state_name, expecting: expecting}

  def close(protocol, reason), do:
    protocol
    |> add_action({:close, reason})
    |> next_state(:closed)

  def transition_after_message(protocol, next_state), do:
    protocol
    |> next_state({:awaiting_message_header, next_state}, 5)
    |> process_buffer()

  def debug_log(%{debug?: false}, _lambda), do: nil
  def debug_log(_protocol, lambda), do: Logger.debug(lambda)


  #-----------------------------------------------------------------------------------------------------------
  # Dispatching of incoming messages
  #-----------------------------------------------------------------------------------------------------------

  defp dispatch_message(%{name: :closed} = protocol, _), do:
    protocol
  defp dispatch_message(%{name: {:awaiting_message_header, next_state_name}} = protocol, raw_message_header) do
    message_header = Messages.decode_message_header(raw_message_header)
    if message_header.length > 0 do
      protocol
      |> next_state({:awaiting_message_payload, next_state_name, message_header.type}, message_header.length)
      |> process_buffer()
    else
      protocol
      |> next_state(next_state_name)
      |> dispatch_decoded_message(message_header.type, nil)
    end
  end
  defp dispatch_message(%{name: {:awaiting_message_payload, next_state_name, message_type}} = protocol, payload), do:
    protocol
    |> next_state(next_state_name)
    |> dispatch_decoded_message(message_type, Messages.decode_message(message_type, payload))
  defp dispatch_message(protocol, message), do:
    dispatch_decoded_message(protocol, nil, message)

  defp dispatch_decoded_message(%{name: :syncing} = protocol, :sync, _), do:
    transition_after_message(protocol, :ready)
  defp dispatch_decoded_message(%{name: :syncing} = protocol, _ignore, _), do:
    transition_after_message(protocol, :syncing)
  defp dispatch_decoded_message(protocol, nil, raw_payload) do
    debug_log(protocol, fn ->
      ["psql server: received ", inspect(raw_payload), " in state ", to_string(protocol.name)]
    end)

    invoke_message_handler(protocol, :raw, raw_payload)
  end
  defp dispatch_decoded_message(protocol, type, payload) do
    log_decoded_message(protocol, type, payload)
    invoke_message_handler(protocol, type, payload)
  end

  defp log_decoded_message(protocol, type, payload), do:
    debug_log(protocol, fn ->
      payload_str = case payload do
        nil -> ""
        other -> inspect(other)
      end

      ["psql server: received ", to_string(type), " ", payload_str]
    end)

  defp invoke_message_handler(protocol, :terminate, _payload), do:
    close(protocol, :normal)
  defp invoke_message_handler(protocol, message_type, payload), do:
    module(protocol.name).handle_client_message(protocol, message_type, payload)

  defp module(:initial), do: Air.PsqlServer.Protocol.Authentication
  defp module(:ssl_negotiated), do: Air.PsqlServer.Protocol.Authentication
  defp module(:login_params), do: Air.PsqlServer.Protocol.Authentication
  defp module(:awaiting_password), do: Air.PsqlServer.Protocol.Authentication
  defp module(:ready), do: Air.PsqlServer.Protocol.QueryExecution
end
