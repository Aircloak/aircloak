defmodule AirSocket.Serializer do
  @moduledoc "Gzip+Json serializer for the socket client."
  @behaviour Phoenix.Channels.GenSocketClient.Serializer

  require Logger


  # -------------------------------------------------------------------
  # Phoenix.Channels.GenSocketClient.Serializer callbacks
  # -------------------------------------------------------------------

  @doc false
  def decode_message(encoded_message) do
    %{"topic" => topic, "event" => event, "payload" => payload, "ref" => ref} =
      encoded_message
      |> :zlib.gunzip()
      |> Poison.decode!()

    %{topic: topic, event: event, payload: payload, ref: ref}
  end

  @doc false
  def encode_message(message) do
    {time, encoded_message} = :timer.tc(fn -> message |> Poison.encode_to_iodata!() |> :zlib.gzip end)

    if time > 10_000 do
      # log decoding times longer than 10ms
      Logger.warn("encoding a message took #{div(time, 1000)}ms, encoded message size=#{byte_size(encoded_message)} bytes")
    end

    {:binary, encoded_message}
  end
end
