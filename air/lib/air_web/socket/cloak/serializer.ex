defmodule AirWeb.Socket.Cloak.Serializer do
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
  def encode!(%Reply{} = reply),
    do:
      encode(%Message{
        join_ref: reply.join_ref,
        ref: reply.ref,
        topic: reply.topic,
        event: "phx_reply",
        payload: %{status: reply.status, response: reply.payload}
      })

  def encode!(%Message{} = msg), do: encode(msg)

  @doc false
  def decode!(message, _opts),
    do:
      Aircloak.report_long(:decode_cloak_message, fn ->
        message |> :erlang.binary_to_term() |> to_message()
      end)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp encode(message),
    do:
      {:socket_push, :binary,
       :erlang.term_to_binary([
         message.join_ref,
         message.ref,
         message.topic,
         message.event,
         message.payload
       ])}

  defp to_message([join_ref, ref, topic, event, payload]),
    do: %Message{join_ref: join_ref, ref: ref, topic: topic, event: event, payload: payload}
end
