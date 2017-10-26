defmodule CentralWeb.Socket.Air.Serializer do
  @moduledoc "Gzip based socket serializer."
  @behaviour Phoenix.Transports.Serializer

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
  def encode!(%Reply{} = reply), do:
    encode(%Message{
      join_ref: reply.join_ref,
      ref: reply.ref,
      topic: reply.topic,
      event: "phx_reply",
      payload: %{status: reply.status, response: reply.payload},
    })
  def encode!(%Message{} = msg),
    do: encode(msg)

  @doc false
  def decode!(message, _opts), do:
    message
    |> :zlib.gunzip()
    |> Poison.decode!()
    |> to_message()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp encode(message) do
    {:socket_push, :binary,
      [message.join_ref, message.ref, message.topic, message.event, message.payload]
      |> Poison.encode_to_iodata!()
      |> :zlib.gzip()
    }
  end

  defp to_message([join_ref, ref, topic, event, payload]), do:
    %Message{join_ref: join_ref, ref: ref, topic: topic, event: event, payload: payload}
end
