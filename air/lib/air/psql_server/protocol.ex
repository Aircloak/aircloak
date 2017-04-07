defmodule Air.PsqlServer.Protocol do
  @moduledoc """
  PostgreSQL server protocol implemented as a state machine.

  This module handles the workflow of the
  [PostgreSQL server protocol v3.0](https://www.postgresql.org/docs/9.6/static/protocol.html).

  The module is implemented as a pure passive state machine, and doesn't deal with
  temporal logic, TCP communication, interaction with the system, or other side-effects.
  These are left to the driver of the module (e.g. TCP server, or a test). For an example
  usage see `Air.PsqlServer.RanchServer`.

  The general usage flow starts by creating the initial protocol state with `new/0`.
  Then, the driver must feed input bytes to the state with `process/2`. As the result,
  the protocol will change its internal state and produce output actions which the driver must
  obtain using `actions/1` and interpret them.
  """

  require Logger
  alias Air.PsqlServer.Protocol.Messages

  @type t :: %{
    state: state,
    syncing?: boolean,
    buffer: binary,
    expecting: non_neg_integer,
    decode_message?: boolean,
    decoded_message_type: Messages.message_header,
    actions: [action],
    describing_statement: nil | binary,
    running_prepared_statement: nil | binary,
    prepared_statements: %{String.t => prepared_statement},
    debug?: boolean,
  }

  @type state ::
    :initial |
    :negotiating_ssl |
    :ssl_negotiated |
    :login_params |
    :authenticating |
    :ready |
    :closed

  @type action ::
    {:send, iodata()} |
    {:close, reason :: any} |
    :upgrade_to_ssl |
    {:login_params, map} |
    {:authenticate, password :: binary} |
    {:run_query, String.t, [%{type: psql_type, value: db_value}], non_neg_integer} |
    {:describe_statement, String.t, [%{type: psql_type, value: db_value}] | nil}

  @type db_value :: String.t | number | boolean | nil

  @type authentication_method :: :cleartext

  @type psql_type ::
    :boolean
    | :int2 | :int4 | :int8
    | :float4 | :float8
    | :numeric
    | :text
    | :date | :time | :timestamp
    | :unknown

  @type column :: %{name: String.t, type: psql_type}

  @type query_result :: %{columns: [column], rows: [db_value]} | nil

  @type prepared_statement :: %{
    name: String.t,
    query: String.t,
    num_params: non_neg_integer,
    param_types: [psql_type],
    parsed_param_types: [psql_type],
    params: [db_value],
    result_codes: nil | [:text | :binary],
    columns: nil | column
  }

  @type event ::
    :ssl_negotiated |
    {:authentication_method, authentication_method} |
    {:authenticated, boolean} |
    {:send_query_result, query_result} |
    {:describe_result, describe_result}

  @type describe_result :: %{error: String.t} | %{columns: [column], param_types: [psql_type]}

  @header_message_bytes 5


  #-----------------------------------------------------------------------------------------------------------
  # Behaviour callbacks
  #-----------------------------------------------------------------------------------------------------------

  @doc "Invoked to handle a client message."
  @callback handle_client_message(t, Messages.client_message_name | :raw, map | binary) :: t

  @doc "Invoked to handle an event issued by the driver (such as TCP process owning this protocol)."
  @callback handle_event(t, event) :: t


  #-----------------------------------------------------------------------------------------------------------
  # API for clients
  #-----------------------------------------------------------------------------------------------------------

  @doc "Creates the initial protocol state."
  @spec new() :: t
  def new() do
    %{
      state: :initial,
      syncing?: false,
      buffer: "",
      expecting: 8,
      decode_message?: false,
      decoded_message_type: nil,
      actions: [],
      describing_statement: nil,
      describing_statement: nil,
      running_prepared_statement: nil,
      prepared_statements: %{},
      debug?: Keyword.get(Application.fetch_env!(:air, Air.PsqlServer), :debug, false)
    }
  end

  @doc """
  Returns the list of actions which the driver must perform.

  This function returns following actions:

  - `{:send, iodata()}` - The driver must send iodata to the client.
  - `{:close, reason :: any}` - The driver must close the connection
  - `{:login_params, map}` - The driver must store login params, and select the
    authentication method with `authentication_method/2`.
  - `{:authenticate, password :: binary}` - The driver must authenticate the user
    identified by `:login_params` and the given password. If the authentication
    succeeded, the driver must invoke `authenticated/1`.
  """
  @spec actions(t) :: {[action], t}
  def actions(protocol), do:
    {Enum.reverse(protocol.actions), %{protocol | actions: []}}

  @doc "Should be invoked by the driver to feed input bytes to the protocol state machine."
  @spec process(t, binary) :: t
  def process(protocol, input), do:
    process_buffer(%{protocol | buffer: protocol.buffer <> input})

  @doc "Should be invoked by the driver after the connection is upgraded to ssl."
  @spec ssl_negotiated(t) :: t
  def ssl_negotiated(protocol), do:
    dispatch_event(protocol, :ssl_negotiated)

  @doc "Should be invoked by the driver to choose the authentication method."
  @spec authentication_method(t, authentication_method) :: t
  def authentication_method(protocol, authentication_method), do:
    dispatch_event(protocol, {:authentication_method, authentication_method})

  @doc "Should be invoked by the driver if the user has been authenticated."
  @spec authenticated(t, boolean) :: t
  def authenticated(protocol, success), do:
    dispatch_event(protocol, {:authenticated, success})

  @doc "Should be invoked by the driver when the select query result is available."
  @spec query_result(t, query_result) :: t
  def query_result(protocol, result), do:
    dispatch_event(protocol, {:send_query_result, result})

  @doc "Should be invoked by the driver when the describe result is available."
  @spec describe_result(t, describe_result) :: t
  def describe_result(protocol, describe_result), do:
    dispatch_event(protocol, {:describe_result, describe_result})

  @doc "Adds a send message action to the list of pending actions."
  @spec send_to_client(t, Messages.server_message) :: t
  def send_to_client(protocol, message) do
    debug_log(protocol, fn -> ["psql server: sending ", inspect(message)] end)
    add_action(protocol, {:send, Messages.encode_message(message)})
  end


  #-----------------------------------------------------------------------------------------------------------
  # API for behaviour callbacks
  #-----------------------------------------------------------------------------------------------------------

  @doc "Adds an action to the list of pending actions."
  @spec add_action(t, action) :: t
  def add_action(protocol, action), do: %{protocol | actions: [action | protocol.actions]}

  @doc "Sets the next state and resets the number of awaiting bytes."
  @spec next_state(t, state) :: t
  def next_state(protocol, next_state), do:
    %{protocol | state: next_state, expecting: 0}

  @doc "Awaits the client message, and optionally decodes it, and changes the state."
  @spec await_client_message(t, [state: state, bytes: pos_integer, decode_message?: boolean]) :: t
  def await_client_message(protocol, opts \\ []), do:
    protocol
    |> next_state(Keyword.get(opts, :state, protocol.state))
    |> Map.put(:expecting, Keyword.get(opts, :bytes, @header_message_bytes))
    |> Map.put(:decode_message?, Keyword.get(opts, :decode?, true))
    |> process_buffer()

  @doc "Puts the protocol into the syncing mode."
  @spec syncing(t) :: t
  def syncing(protocol), do: %{protocol | syncing?: true}

  @doc "Puts the protocol into the closed state."
  @spec close(t, any) :: t
  def close(protocol, reason), do:
    protocol
    |> add_action({:close, reason})
    |> next_state(:closed)


  #-----------------------------------------------------------------------------------------------------------
  # Dispatching of incoming messages
  #-----------------------------------------------------------------------------------------------------------

  defp process_buffer(%{expecting: expecting, buffer: buffer} = protocol)
      when expecting > 0 and byte_size(buffer) >= expecting do
    <<message::binary-size(expecting)>> <> rest_buffer = buffer

    %{protocol | expecting: 0, buffer: rest_buffer}
    |> dispatch_message(message)
    |> process_buffer()
  end
  defp process_buffer(protocol), do: protocol

  defp dispatch_message(%{state: :closed} = protocol, _), do:
    protocol
  defp dispatch_message(%{decode_message?: true, decoded_message_type: nil} = protocol, raw_message_header) do
    message_header = Messages.decode_message_header(raw_message_header)
    if message_header.length > 0 do
      %{protocol | decoded_message_type: message_header.type}
      |> await_client_message(bytes: message_header.length)
      |> process_buffer()
    else
      dispatch_message(protocol, message_header.type, nil)
    end
  end
  defp dispatch_message(%{decode_message?: true} = protocol, payload), do:
      dispatch_message(
        protocol,
        protocol.decoded_message_type,
        Messages.decode_message(protocol.decoded_message_type, payload)
      )
  defp dispatch_message(protocol, message), do:
    dispatch_message(protocol, :raw, message)

  defp dispatch_message(protocol, type, payload) do
    protocol = %{protocol | decode_message?: false, decoded_message_type: nil}
    log_decoded_message(protocol, type, payload)
    invoke_message_handler(protocol, type, payload)
  end

  defp log_decoded_message(protocol, type, payload), do:
    debug_log(protocol, fn ->
      payload_str = case payload do
        nil -> ""
        other -> inspect(other)
      end

      ["psql server: received ", to_string(type), " ", payload_str]
    end)

  defp invoke_message_handler(protocol, :terminate, _payload), do:
    close(protocol, :normal)
  defp invoke_message_handler(%{syncing?: true} = protocol, :sync, _), do:
    await_client_message(%{protocol | syncing?: false}, state: :ready)
  defp invoke_message_handler(%{syncing?: true} = protocol, _ignore, _), do:
    protocol
    |> syncing()
    |> await_client_message()
  defp invoke_message_handler(protocol, message_type, payload), do:
    protocol_handler(protocol.state).handle_client_message(protocol, message_type, payload)


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp debug_log(%{debug?: false}, _lambda), do: nil
  defp debug_log(_protocol, lambda), do: Logger.debug(lambda)

  defp protocol_handler(state) when state in [
    :initial, :negotiating_ssl, :ssl_negotiated, :login_params, :authenticating
    ] do
    Air.PsqlServer.Protocol.Authentication
  end
  defp protocol_handler(:ready), do:
    Air.PsqlServer.Protocol.QueryExecution

  defp dispatch_event(protocol, event), do:
    protocol_handler(protocol.state).handle_event(protocol, event)
end
