defmodule Air.CloakSocketSerializer do
  @moduledoc """
  Phoenix socket client serializer.

  Messages are serialized as gzipped Erlang term formats. The main reason for this is performance. ETF encoding/decoding
  runs much faster than e.g. JSON encoder, and this is significant for larger messages, for example when large result of
  a query is sent from the cloak to the air. As an added bonus, there's no impedance mismatch, so we can send atoms,
  tuples and other kind of Erlang specific data to the other side just like when using clustered Erlang.
  """
  @behaviour Phoenix.Channels.GenSocketClient.Serializer

  require Logger


  # -------------------------------------------------------------------
  # Phoenix.Channels.GenSocketClient.Serializer callbacks
  # -------------------------------------------------------------------

  @doc false
  def decode_message(encoded_message), do:
    encoded_message
    |> :zlib.gunzip()
    |> :erlang.binary_to_term()
    |> Map.take([:topic, :event, :payload, :ref])
    |> adapt_first_phoenix_reply()

  @doc false
  def encode_message(message) do
    {time, encoded_message} = :timer.tc(fn -> message |> :erlang.term_to_binary() |> :zlib.gzip() end)

    if time > 10_000 do
      # log decoding times longer than 10ms
      Logger.warn(
        "encoding a message took #{div(time, 1000)}ms, encoded message size=#{byte_size(encoded_message)} bytes"
      )
    else
      Logger.debug(
        "encoding a message took #{div(time, 1000)}ms, encoded message size=#{byte_size(encoded_message)} bytes"
      )
    end

    {:binary, encoded_message}
  end


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
