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

  alias Air.PsqlServer.Protocol.Messages
  require Logger

  @opaque t :: %{
    name: atom,
    buffer: binary,
    expecting: non_neg_integer,
    actions: [action],
    prepared_statements: %{String.t => prepared_statement}
  }

  @type action ::
    {:send, iodata()} |
    {:close, reason :: any} |
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

  @type describe_result :: %{error: String.t} | %{columns: [column], param_types: [psql_type]}


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc "Creates the initial protocol state."
  @spec new() :: t
  def new() do
    %{
      name: :initial,
      buffer: "",
      expecting: 8,
      actions: [],
      prepared_statements: %{}
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
  def actions(state), do:
    {Enum.reverse(state.actions), %{state | actions: []}}

  @doc "Should be invoked by the driver after the connection is upgraded to ssl."
  @spec ssl_negotiated(t) :: t
  def ssl_negotiated(state), do:
    dispatch_event(state, :ssl_negotiated)

  @doc "Should be invoked by the driver to feed input bytes to the protocol state machine."
  @spec process(t, binary) :: t
  def process(state, input), do:
    process_buffer(%{state | buffer: state.buffer <> input})

  @doc "Should be invoked by the driver to choose the authentication method."
  @spec authentication_method(t, authentication_method) :: t
  def authentication_method(state, authentication_method), do:
    dispatch_event(state, {:authentication_method, authentication_method})

  @doc "Should be invoked by the driver if the user has been authenticated."
  @spec authenticated(t, boolean) :: t
  def authenticated(state, success), do:
    dispatch_event(state, {:authenticated, success})

  @doc "Should be invoked by the driver when the select query result is available."
  @spec query_result(t, query_result) :: t
  def query_result(state, result), do:
    dispatch_event(state, {:query_result, result})

  @doc "Should be invoked by the driver when the describe result is available."
  @spec describe_result(t, describe_result) :: t
  def describe_result(state, describe_result), do:
    dispatch_event(state, {:describe_result, describe_result})


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp process_buffer(%{expecting: expecting, buffer: buffer} = state)
      when expecting > 0 and byte_size(buffer) >= expecting do
    <<message::binary-size(expecting)>> <> rest_buffer = buffer

    %{state | expecting: 0, buffer: rest_buffer}
    |> dispatch_event({:message, message})
    |> process_buffer()
  end
  defp process_buffer(state), do: state

  defp send_to_client(state, message, args \\ []) do
    Logger.debug(fn ->
      ["psql server: sending ", to_string(message), " ", inspect(args)]
    end)
    add_action(state, {:send, apply(Messages, message, args)})
  end

  defp add_action(state, action), do: %{state | actions: [action | state.actions]}

  defp next_state(state, next_state_name, expecting \\ 0), do:
    %{state | name: next_state_name, expecting: expecting}

  defp close(state, reason), do:
    state
    |> add_action({:close, reason})
    |> next_state(:closed)

  defp transition_after_message(state, next_state), do:
    state
    |> next_state({:message_header, next_state}, 5)
    |> process_buffer()


  #-----------------------------------------------------------------------------------------------------------
  # Handling events
  #-----------------------------------------------------------------------------------------------------------

  defp dispatch_event(state, event), do:
    handle_event(state, state.name, event)

  defp dispatch_client_message(state, type, payload \\ nil) do
    Logger.debug(fn ->
      payload_str = case payload do
        nil -> ""
        other -> inspect(other)
      end

      ["psql server: received ", to_string(type), " ", payload_str]
    end)

    dispatch_event(state, {:message, %{type: type, payload: payload}})
  end

  # :closed -> ignore all actions
  defp handle_event(state, :closed, _), do:
    state
  # :initial -> awaiting SSLRequest or StartupMessage
  defp handle_event(state, :initial, {:message, message}) do
    if Messages.ssl_message?(message) do
      state
      |> send_to_client(:require_ssl)
      |> add_action(:upgrade_to_ssl)
      |> next_state(:ssl)
    else
      state
      |> send_to_client(:fatal_error_message, ["Only SSL connections are allowed!"])
      |> close(:required_ssl)
    end
  end
  # :message_header -> awaiting a message header
  defp handle_event(state, {:message_header, next_state_name}, {:message, raw_message_header}) do
    message_header = Messages.decode_message_header(raw_message_header)
    if message_header.length > 0 do
      state
      |> next_state({:message_payload, next_state_name, message_header.type}, message_header.length)
      |> process_buffer()
    else
      state
      |> next_state(next_state_name)
      |> dispatch_client_message(message_header.type)
    end
  end
  # :message_payload -> awaiting a message payload
  defp handle_event(state, {:message_payload, next_state_name, message_type}, {:message, payload}), do:
    state
    |> next_state(next_state_name)
    |> dispatch_client_message(message_type, Messages.decode_message(message_type, payload))
  # :ssl -> waiting for the connection to be upgraded to SSL
  defp handle_event(state, :ssl, :ssl_negotiated), do:
    next_state(state, :startup_message, 8)
  # :startup_message -> expecting startup message from the client
  defp handle_event(state, :startup_message, {:message, message}) do
    startup_message = Messages.decode_startup_message(message)
    if startup_message.version.major != 3 do
      close(state, :unsupported_protocol_version)
    else
      next_state(state, :login_params, startup_message.length)
    end
  end
  # :login_params -> expecting login params from the client
  defp handle_event(state, :login_params, {:message, raw_login_params}), do:
    state
    |> add_action({:login_params, Messages.decode_login_params(raw_login_params)})
    |> next_state(:authentication_method)
  # :authentication_method -> expecting the driver to choose the authentication method
  defp handle_event(state, :authentication_method, {:authentication_method, authentication_method}), do:
    state
    |> send_to_client(:authentication_method, [authentication_method])
    |> transition_after_message(:password)
  # :password -> expecting password from the client
  defp handle_event(state, :password, {:message, %{type: :password} = password_message}), do:
    state
    |> add_action({:authenticate, password_message.payload})
    |> next_state(:authenticating)
  # :authenticating -> expecting authentication result from the driver
  defp handle_event(state, :authenticating, {:authenticated, true}) do
    state
    |> send_to_client(:authentication_ok)
    |> send_to_client(:parameter_status, ["application_name", "aircloak"])
    |> send_to_client(:parameter_status, ["server_version", "1.0.0"])
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:ready)
  end
  defp handle_event(state, :authenticating, {:authenticated, false}), do:
    state
    # We're sending AuthenticationOK to indicate to the client that the auth procedure went fine. Then
    # we'll send a fatal error with a custom error message. It is unclear from the official docs that it
    # should be done this way. However, this approach produces a nicer error message, and it's the same
    # in PostgreSQL server (determined by wireshark).
    |> send_to_client(:authentication_ok)
    |> send_to_client(:fatal_error_message, ["Authentication failed!"])
    |> close(:not_authenticated)
  # :ready -> handling of various client messages
  defp handle_event(state, :ready, {:message, message}), do:
    handle_ready_message(state, message.type, message.payload)
  # :running_query -> awaiting query result
  defp handle_event(state, :running_query, {:query_result, result}), do:
    state
    |> send_result(result)
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:ready)
  # :describing_statement -> awaiting describe result
  defp handle_event(state, {:describing_statement, name}, {:describe_result, description}) do
    if Map.has_key?(description, :error) do
      state
      |> send_to_client(:syntax_error_message, [description.error])
      |> transition_after_message(:ready)
    else
      prepared_statement = Map.fetch!(state.prepared_statements, name)

      result_codes = prepared_statement.result_codes || [:text]
      state
      |> put_in([:prepared_statements, name, :parsed_param_types], description.param_types)
      |> put_in([:prepared_statements, name, :columns], description.columns)
      |> send_parameter_descriptions(prepared_statement, description.param_types)
      |> send_to_client(:row_description, [description.columns, result_codes])
      |> transition_after_message(:ready)
    end
  end
  # :running_prepared_statement -> awaiting result of an executed prepared statement
  defp handle_event(state, {:running_prepared_statement, name}, {:query_result, result}) do
    statement = Map.fetch!(state.prepared_statements, name)

    state
    |> send_rows(result.rows, statement.columns, statement.result_codes)
    |> send_to_client(:command_complete, ["SELECT #{length(result.rows)}"])
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:syncing)
  end

  # :syncing -> ignoring all message until sync arrives
  defp handle_event(state, :syncing, {:message, %{type: :sync}}), do:
    transition_after_message(state, :ready)
  defp handle_event(state, :syncing, {:message, _}), do:
    transition_after_message(state, :syncing)


  #-----------------------------------------------------------------------------------------------------------
  # Handling of messages in the `:ready` state
  #-----------------------------------------------------------------------------------------------------------

  defp handle_ready_message(state, :flush, _), do:
    transition_after_message(state, :ready)
  defp handle_ready_message(state, :sync, _), do:
    state
    |> send_to_client(:ready_for_query)
    |> transition_after_message(:ready)
  defp handle_ready_message(state, :terminate, _), do:
    close(state, :normal)
  defp handle_ready_message(state, :query, payload), do:
    state
    |> add_action({:run_query, payload, [], 0})
    |> next_state(:running_query)
  defp handle_ready_message(state, :parse, prepared_statement) do
    prepared_statement = Map.merge(
      prepared_statement,
      %{params: nil, parsed_param_types: [], result_codes: nil, columns: nil}
    )

    state
    |> put_in([:prepared_statements, prepared_statement.name], prepared_statement)
    |> send_to_client(:parse_complete)
    |> transition_after_message(:ready)
  end
  defp handle_ready_message(state, :bind, bind_data) do
    prepared_statement = Map.fetch!(state.prepared_statements, bind_data.name)

    param_types =
      case prepared_statement.param_types do
        [_|_] -> prepared_statement.param_types
        [] -> prepared_statement.parsed_param_types
      end

    params = Messages.convert_params(bind_data.params, bind_data.format_codes, param_types)

    state
    |> put_in([:prepared_statements, bind_data.name],
        %{prepared_statement | params: params, result_codes: bind_data.result_codes})
    |> send_to_client(:bind_complete)
    |> transition_after_message(:ready)
  end
  defp handle_ready_message(state, :describe, describe_data) do
    prepared_statement = Map.fetch!(state.prepared_statements, describe_data.name)

    state
    |> add_action({:describe_statement, prepared_statement.query, params_with_types(prepared_statement)})
    |> next_state({:describing_statement, describe_data.name})
  end
  defp handle_ready_message(state, :execute, execute_data) do
    prepared_statement = Map.fetch!(state.prepared_statements, execute_data.name)

    state
    |> add_action({:run_query, prepared_statement.query, params_with_types(prepared_statement),
      execute_data.max_rows})
    |> next_state({:running_prepared_statement, execute_data.name})
  end
  defp handle_ready_message(state, :close, close_data), do:
    state
    |> update_in([:prepared_statements], &Map.delete(&1, close_data.name))
    |> send_to_client(:close_complete)
    |> transition_after_message(:ready)


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp send_parameter_descriptions(state, %{params: nil}, param_types), do:
    # parameters are not bound -> send parameter descriptions
    send_to_client(state, :parameter_description, [param_types])
  defp send_parameter_descriptions(state, _, _), do:
    # parameters are already bound -> client is not expecting parameter descriptions
    state

  defp send_result(state, nil), do:
    send_to_client(state, :command_complete, [""])
  defp send_result(state, %{rows: rows, columns: columns}), do:
    state
    |> send_to_client(:row_description, [columns, [:text]])
    |> send_rows(rows, columns, [:text])
    |> send_to_client(:command_complete, ["SELECT #{length(rows)}"])
  defp send_result(state, %{error: error}), do:
    send_to_client(state, :syntax_error_message, [error])

  defp send_rows(state, rows, columns, formats), do:
    Enum.reduce(rows, state, &send_to_client(&2, :data_row, [&1, column_types(columns), formats]))

  defp column_types(nil), do: Stream.cycle([:text])
  defp column_types(columns), do: Enum.map(columns, &(&1.type))

  defp params_with_types(%{params: nil}), do:
    nil
  defp params_with_types(prepared_statement) do
    param_types =
      cond do
        match?([_|_], prepared_statement.param_types) -> prepared_statement.param_types
        match?([_|_], prepared_statement.parsed_param_types) -> prepared_statement.parsed_param_types
        true -> Stream.cycle([:unknown])
      end

    Enum.zip(param_types, prepared_statement.params)
  end
end
