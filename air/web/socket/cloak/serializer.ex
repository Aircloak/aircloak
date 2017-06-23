defmodule Air.Socket.Cloak.Serializer do
  @moduledoc "Gzip based socket serializer."
  @behaviour Phoenix.Transports.Serializer

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
  def decode!(message, _opts) do
    {time, result} = :timer.tc(fn ->
      struct(Phoenix.Socket.Message,
        message
        |> :zlib.gunzip()
        |> :erlang.binary_to_term()
      )
    end)

    if time > 10_000 do
      # log decoding times longer than 10ms
      Logger.warn("decoding of a cloak message took #{div(time, 1000)}ms, raw message size=#{byte_size(message)} bytes")
    end

    result
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp encode(message) do
    {:socket_push, :binary,
      message
      |> Poison.encode_to_iodata!()
      |> :zlib.gzip()
    }
  end
end
