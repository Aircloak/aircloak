defmodule Air.Socket.Cloak.Serializer do
  @moduledoc """
  Phoenix socket server serializer.

  Messages are serialized as gzipped Erlang term formats. The main reason for this is performance. ETF encoding/decoding
  runs much faster than e.g. JSON encoder, and this is significant for larger messages, for example when large result of
  a query is sent from the cloak to the air. As an added bonus, there's no impedance mismatch, so we can send atoms,
  tuples and other kind of Erlang specific data to the other side just like when using clustered Erlang.
  """

  require Logger
  alias Phoenix.Socket.Reply
  alias Phoenix.Socket.Message
  alias Phoenix.Socket.Broadcast


  # -------------------------------------------------------------------
  # Phoenix.Transports.Serializer callbacks
  # -------------------------------------------------------------------

  @doc false
  def fastlane!(%Broadcast{} = msg) do
    encode(%Message{topic: msg.topic, event: msg.event, payload: msg.payload})
  end

  @doc false
  def encode!(%Reply{} = reply) do
    encode(%Message{topic: reply.topic, event: "phx_reply", ref: reply.ref,
      payload: %{status: reply.status, response: reply.payload}})
  end
  def encode!(%Message{} = msg),
    do: encode(msg)

  @doc false
  def decode!(message, _opts), do:
    Aircloak.report_long(
      :decode_cloak_message,
      fn -> struct(Phoenix.Socket.Message, :erlang.binary_to_term(message)) end
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp encode(message), do:
    {:socket_push, :binary, :erlang.term_to_binary(message)}
end
