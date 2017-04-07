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

  alias Air.PsqlServer.Protocol.{Authentication, Helpers, QueryExecution}

  @opaque t :: %{
    state: atom,
    buffer: binary,
    expecting: non_neg_integer,
    actions: [action],
    prepared_statements: %{String.t => prepared_statement},
    debug?: boolean,
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
      state: :initial,
      buffer: "",
      expecting: 8,
      actions: [],
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
    Helpers.actions(protocol)

  @doc "Should be invoked by the driver to feed input bytes to the protocol state machine."
  @spec process(t, binary) :: t
  def process(protocol, input), do:
    Helpers.process_buffer(%{protocol | buffer: protocol.buffer <> input})

  @doc "Should be invoked by the driver after the connection is upgraded to ssl."
  @spec ssl_negotiated(t) :: t
  def ssl_negotiated(protocol), do:
    Authentication.ssl_negotiated(protocol)

  @doc "Should be invoked by the driver to choose the authentication method."
  @spec authentication_method(t, authentication_method) :: t
  def authentication_method(protocol, authentication_method), do:
    Authentication.authentication_method(protocol, authentication_method)

  @doc "Should be invoked by the driver if the user has been authenticated."
  @spec authenticated(t, boolean) :: t
  def authenticated(protocol, success), do:
    Authentication.authenticated(protocol, success)

  @doc "Should be invoked by the driver when the select query result is available."
  @spec query_result(t, query_result) :: t
  def query_result(protocol, result), do:
    QueryExecution.send_query_result(protocol, result)

  @doc "Should be invoked by the driver when the describe result is available."
  @spec describe_result(t, describe_result) :: t
  def describe_result(protocol, describe_result), do:
    QueryExecution.send_describe_result(protocol, describe_result)
end
