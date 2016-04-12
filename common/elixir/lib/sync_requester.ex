defmodule Air.SyncRequester do
  @moduledoc """
  Helper for issuing synchronous requests over an asynchronous connection (e.g.
  websocket).

  You can use this module to create and tag requests, and handle corresponding
  responses.

  ## Usage

  On the client side you need to create the requester process with `start_link/1`.
  Then you can create a request with `encode_request/3`. The result is a map
  ready to be encoded to textual format (e.g. JSON). The client now needs to send
  this payload to the server.

  When you receive a message on the server, you can decode it with `decode_request/1`.
  The result will be in form of `{request_id, request_payload}`. When you want
  to send a response, you can use `encode_response/2`, where the first argument
  must be the corresponding `request_id`. The result is again a map ready to be
  encoded to textual format which needs to be sent to the client.

  On the client side, the response can be decoded with `decode_response/2`. The
  result will be in form of `{request, request_meta, response}`. The first two
  elements are the ones passed to `encode_request/3` when the request was first
  issued.

  In other words, the requester stores the request data, and then connects
  the response to the original request data. This allows the client to get the
  full context of the request at the time the response arrives.

  Finally, if you wish to tag requests and responses, (for example in Phoenix
  channels messages), you can use `request_event/1` and `response_event/1`
  macros.
  """

  use GenServer
  require Logger

  @type requester_name :: atom
  @type request_id :: String.t
  @type request :: any
  @type meta :: any
  @type response :: any
  @type payload :: %{String.t => String.t}


  # -------------------------------------------------------------------
  # API functions
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
  Generates a unique string which identifies a response.

  Implemented as a macro, so it can be safely used in pattern matches.
  """
  defmacro response_event(command) do
    quote do
      unquote("#{__MODULE__}.Response") <> unquote(command)
    end
  end

  @doc """
  Starts the requester process.

  This process is used only as an owner of the underlying ETS table where
  pending requests are stored.
  """
  @spec start_link(requester_name) :: GenServer.on_start
  def start_link(requester_name) do
    GenServer.start_link(__MODULE__, requester_name)
  end

  @doc """
  Encodes the given request.

  Optional meta can be given. This data is stored locally together with the
  request data, but it is not passed to the server. This data is extracted
  while decoding a response so it can be used to keep some context which you
  don't want to send to the server.
  """
  @spec encode_request(requester_name, request, meta) :: payload
  def encode_request(requester_name, request, meta \\ nil) do
    id = next_id(requester_name)
    store_request(requester_name, id, request, meta)
    encode_payload({__MODULE__, :request, id, request})
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

  @doc """
  Decodes the response.

  This function will also lookup the request data in a local ETS table and return
  associated meta and request which was passed to `encode_response/2`. If the
  data is not available, `nil` values are returned. This will happen if the
  requester process has been restarted.
  """
  @spec decode_response!(requester_name, payload) :: {request | nil, meta | nil, response}
  def decode_response!(requester_name, payload) do
    {__MODULE__, :response, id, response} = decode_payload!(payload)

    case :ets.take(table(requester_name), {:request, id}) do
      [{_, %{request: request, meta: request_meta}}] ->
        :ets.delete(table(requester_name), {:id, id})
        {request, request_meta, response}
      [] -> {nil, nil, response}
    end
  end


  def init(requester_name) do
    :ets.new(table(requester_name), [
          :set,
          :public,
          :named_table,
          write_concurrency: true,
          read_concurrency: true
        ])

    {:ok, nil}
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

  defp next_id(requester_name) do
    new_id = Base.encode64(:crypto.rand_bytes(32))
    if :ets.insert_new(table(requester_name), {{:id, new_id}}) do
      new_id
    else
      next_id(table(requester_name))
    end
  end

  defp store_request(requester_name, id, request, meta) do
    :ets.insert(table(requester_name), {{:request, id}, %{request: request, meta: meta}})
  end

  defp table(requester_name), do: Module.concat(__MODULE__, requester_name)
end
