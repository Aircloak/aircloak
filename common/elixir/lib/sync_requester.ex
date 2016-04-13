defmodule Air.SyncRequester do
  @moduledoc """
  Helper for issuing synchronous requests over an asynchronous connection (e.g.
  websocket).

  You can use this module to create and tag requests, and handle corresponding
  responses.

  ## API

  The API of this module is divided into client-side and server-side functions.

  On the client side, you need to provide the backend module which is used to
  store the outgoing requests, and match incoming responses to previous requests.
  The module `Air.SyncRequester.Backend.Ets` implements an ETS powered backend.

  The server-side API consists of plain functions and does not require a backend.
  These functions are simply used to decode requests, and later encode
  corresponding responses.

  ## Request-response cycle

  You can create a request with `encode_request/4`. The result is a map ready to
  be encoded to textual format (e.g. JSON). The client now needs to send this
  payload to the server.

  When you receive a message on the server, you can decode it with `decode_request/1`.
  The result will be in form of `{request_id, request_payload}`. When you want
  to send a response, you can use `encode_response/2`, where the first argument
  must be the corresponding `request_id`. The result is again a map ready to be
  encoded to textual format which needs to be sent to the client.

  On the client side, the response can be decoded with `decode_response/3`. This
  function will also match the response to the previous request and return the
  original request data.

  Finally, if you wish to tag requests and responses, (for example in Phoenix
  channels messages), you can use `request_event/1` and `response_event/1`
  macros.
  """

  require Logger

  @type backend_arg :: any
  @type request_id :: String.t
  @type request :: any
  @type meta :: any
  @type response :: any
  @type payload :: %{String.t => String.t}


  # -------------------------------------------------------------------
  # Client-side API functions
  # -------------------------------------------------------------------

  @doc """
  Generates a unique string which identifies a request.
  Implemented as a macro, so it can be safely used in pattern matches.
  """
  defmacro request_event(command) do
    quote do
      unquote("#{__MODULE__}.Request") <> unquote(command)
    end
  end

  @doc """
  Encodes the given request.

  Optional meta can be given. This data is stored in the backend together with the
  request data, but it is not passed to the server. This data is extracted
  while decoding a response so it can be used to keep some context which you
  don't want to send to the server.
  """
  @spec encode_request(module, backend_arg, request, meta) :: payload
  def encode_request(backend_mod, backend_arg, request, meta \\ nil) do
    id =
      # A combination of the issuer pid, monotonic (non strictly unique) integer, and some randomness should
      # be pretty unique.
      {self(), :erlang.monotonic_time(), :crypto.rand_bytes(32)}
      |> :erlang.term_to_binary()
      |> Base.encode64()

    :ok = backend_mod.store_request(backend_arg, id, request, meta)
    encode_payload({__MODULE__, :request, id, request})
  end

  @doc """
  Decodes the response.

  This function will decode the response, and match it to the original request.
  If the request is found, the tuple `{:matched, {request, meta, response}}` is
  returned. Otherwise, the result is `{:not_matched, response}`.
  """
  @spec decode_response!(module, backend_arg, payload) ::
      {:matched, {request, meta, response}} |
      {:not_matched, response}
  def decode_response!(backend_mod, backend_arg, payload) do
    {__MODULE__, :response, id, response} = decode_payload!(payload)

    case backend_mod.pop_request(backend_arg, id) do
      {:ok, request, meta} ->
        {:matched, {request, meta, response}}
      {:error, _} ->
        {:not_matched, response}
    end
  end


  # -------------------------------------------------------------------
  # Server-side API functions
  # -------------------------------------------------------------------

  @doc """
  Generates a unique string which identifies a response.

  Implemented as a macro, so it can be safely used in pattern matches.
  """
  defmacro response_event(command) do
    quote do
      unquote("#{__MODULE__}.Response") <> unquote(command)
    end
  end


  @doc """
  Encodes the response.

  The `request_id` must be the one obtained by `decode_request!/1`.
  """
  @spec encode_response(request_id, response) :: payload
  def encode_response(id, response),
    do: encode_payload({__MODULE__, :response, id, response})


  @doc """
  Decodes the request.

  Returns a tuple with a `request_id` and a request payload. To respond to the
  request use `encode_response/2` passing the same `request_id`.
  """
  @spec decode_request!(payload) :: {request_id, request}
  def decode_request!(payload) do
    {__MODULE__, :request, id, request} = decode_payload!(payload)
    {id, request}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp encode_payload(payload) do
    %{
      "payload" =>
        payload
        |> :erlang.term_to_binary()
        |> Base.encode64()
    }
  end

  defp decode_payload!(%{"payload" => payload}) do
    payload
    |> Base.decode64!()
    |> :erlang.binary_to_term
  end
end
