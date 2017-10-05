defmodule Air.CloakSocketSerializer do
  @moduledoc """
  Phoenix socket client serializer.

  Messages are serialized as gzipped Erlang term formats. The main reason for this is performance. ETF encoding/decoding
  runs much faster than e.g. JSON encoder, and this is significant for larger messages, for example when large result of
  a query is sent from the cloak to the air. As an added bonus, there's no impedance mismatch, so we can send atoms,
  tuples and other kind of Erlang specific data to the other side just like when using clustered Erlang.
  """

  require Logger

  alias Phoenix.Channels.GenSocketClient.Serializer
  @behaviour Serializer


  # -------------------------------------------------------------------
  # Phoenix.Channels.GenSocketClient.Serializer callbacks
  # -------------------------------------------------------------------

  @impl Serializer
  def decode_message(encoded_message), do:
    encoded_message
    |> :erlang.binary_to_term()
    |> Map.take([:topic, :event, :payload, :ref])
    |> adapt_first_phoenix_reply()

  @impl Serializer
  def encode_message(message), do:
    Aircloak.report_long(:encode_cloak_message, fn -> {:ok, {:binary, :erlang.term_to_binary(message)}} end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp adapt_first_phoenix_reply(%{event: "phx_reply", ref: 1} = message), do:
    # phoenix_gen_socket_client expects string keys for status and response, and a string value for response.
    update_in(message.payload, &%{"status" => to_string(&1.status), "response" => &1.response})
  defp adapt_first_phoenix_reply(other), do:
    # For all other cases, we'll leave atomized keys.
    other
end
