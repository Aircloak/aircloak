defmodule Air.CloakSocketSerializer do
  @moduledoc "Gzip+Json serializer for the cloak socket client."
  @behaviour Phoenix.Channels.GenSocketClient.Serializer

  require Logger


  # -------------------------------------------------------------------
  # Phoenix.Channels.GenSocketClient.Serializer callbacks
  # -------------------------------------------------------------------

  @doc false
  def decode_message(encoded_message) do
    phoenix_message =
      encoded_message
      |> :zlib.gunzip()
      |> :erlang.binary_to_term()

    phoenix_message
    |> Map.take([:topic, :event, :payload, :ref])
    |> adapt_first_phx_reply()
  end

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

  defp adapt_first_phx_reply(%{event: "phx_reply", ref: 1} = message), do:
    # phoenix_gen_socket_client expects string keys for status and response, and a string value for response.
    update_in(message.payload, &%{"status" => to_string(&1.status), "response" => &1.response})
  defp adapt_first_phx_reply(other), do:
    # For all other cases, we'll leave atomized keys.
    other
end
