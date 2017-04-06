defmodule Air.PsqlServer.Protocol.Helpers do
  @moduledoc false

  # Helper functions for working with protocol state, such as dispatching client messages, moving to the next
  # state, awaiting for a message, ...

  require Logger
  alias Air.PsqlServer.Protocol.Messages

  def actions(state), do:
    {Enum.reverse(state.actions), %{state | actions: []}}

  def process_buffer(%{expecting: expecting, buffer: buffer} = state)
      when expecting > 0 and byte_size(buffer) >= expecting do
    <<message::binary-size(expecting)>> <> rest_buffer = buffer

    %{state | expecting: 0, buffer: rest_buffer}
    |> dispatch_message(message)
    |> process_buffer()
  end
  def process_buffer(state), do: state

  def send_to_client(state, message, args \\ []) do
    debug_log(state, fn ->
      ["psql server: sending ", to_string(message), " ", inspect(args)]
    end)
    add_action(state, {:send, apply(Messages, message, args)})
  end

  def add_action(state, action), do: %{state | actions: [action | state.actions]}

  def next_state(state, next_state_name, expecting \\ 0), do:
    %{state | name: next_state_name, expecting: expecting}

  def close(state, reason), do:
    state
    |> add_action({:close, reason})
    |> next_state(:closed)

  def transition_after_message(state, next_state), do:
    state
    |> next_state({:awaiting_message_header, next_state}, 5)
    |> process_buffer()

  def debug_log(%{debug?: false}, _lambda), do: nil
  def debug_log(_state, lambda), do: Logger.debug(lambda)


  #-----------------------------------------------------------------------------------------------------------
  # Dispatching of incoming messages
  #-----------------------------------------------------------------------------------------------------------

  defp dispatch_message(%{name: :closed} = state, _), do:
    state
  defp dispatch_message(%{name: {:awaiting_message_header, next_state_name}} = state, raw_message_header) do
    message_header = Messages.decode_message_header(raw_message_header)
    if message_header.length > 0 do
      state
      |> next_state({:awaiting_message_payload, next_state_name, message_header.type}, message_header.length)
      |> process_buffer()
    else
      state
      |> next_state(next_state_name)
      |> dispatch_decoded_message(message_header.type, nil)
    end
  end
  defp dispatch_message(%{name: {:awaiting_message_payload, next_state_name, message_type}} = state, payload), do:
    state
    |> next_state(next_state_name)
    |> dispatch_decoded_message(message_type, Messages.decode_message(message_type, payload))
  defp dispatch_message(state, message), do:
    dispatch_decoded_message(state, nil, message)

  defp dispatch_decoded_message(%{name: :syncing} = state, :sync, _), do:
    transition_after_message(state, :ready)
  defp dispatch_decoded_message(%{name: :syncing} = state, _ignore, _), do:
    transition_after_message(state, :syncing)
  defp dispatch_decoded_message(state, nil, raw_payload) do
    debug_log(state, fn ->
      ["psql server: received ", inspect(raw_payload), " in state ", to_string(state.name)]
    end)

    invoke_message_handler(state, :"handle_raw_message", raw_payload)
  end
  defp dispatch_decoded_message(state, type, payload) do
    debug_log(state, fn ->
      payload_str = case payload do
        nil -> ""
        other -> inspect(other)
      end

      ["psql server: received ", to_string(type), " ", payload_str]
    end)

    invoke_message_handler(state, :"handle_#{type}_message", payload)
  end

  defp invoke_message_handler(state, fun_atom, payload) do
    apply(module(state.name, fun_atom), fun_atom, [state, state.name, payload])
  end

  defp module(_, :handle_terminate_message), do: Air.PsqlServer.Protocol.Authentication
  defp module(:initial, _), do: Air.PsqlServer.Protocol.Authentication
  defp module(:ssl_negotiated, _), do: Air.PsqlServer.Protocol.Authentication
  defp module(:login_params, _), do: Air.PsqlServer.Protocol.Authentication
  defp module(:awaiting_password, _), do: Air.PsqlServer.Protocol.Authentication
  defp module(:ready, _), do: Air.PsqlServer.Protocol.QueryExecution
end
