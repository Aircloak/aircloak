defmodule Air.PsqlServer.ShadowDb do
  @moduledoc """
  Service for managing shadow databases.

  A shadow database is a local PostgreSQL database which corresponds in structure to data source.
  We maintain these databases so we can offload PostgreSQL specific queries issued over the psql interface.
  """

  use GenServer

  require Logger
  require Aircloak.DeployConfig
  alias Air.PsqlServer.ShadowDb.{Connection, Schema}

  @type connection_params :: %{
          host: String.t(),
          port: non_neg_integer,
          ssl: boolean,
          user: String.t(),
          password: String.t(),
          name: String.t()
        }

  @idle_timeout :timer.seconds(30)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns connection parameters to the shadow database server."
  @spec connection_params() :: connection_params
  def connection_params() do
    default_connection_params()
    |> Map.merge(Aircloak.DeployConfig.get("shadow_database", %{}))
    |> Map.take(~w(host port ssl user password name))
    |> Aircloak.atomize_keys()
  end

  @doc "Executes the query on the given data source."
  @spec query(User.t(), String.t(), String.t(), [term]) :: Connection.query_result()
  def query(user, data_source_name, query, params) do
    GenServer.call(__MODULE__, {:query, user, data_source_name, query, params})
  end

  @doc "Parses the query on the given data source."
  @spec parse(User.t(), String.t(), String.t()) :: Connection.parse_result()
  def parse(user, data_source_name, query) do
    GenServer.call(__MODULE__, {:parse, user, data_source_name, query})
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_arg),
    do: {:ok, %{conn: nil, last_db: nil}}

  @impl GenServer
  def handle_call(
        {:query, user, data_source_name, query, params},
        _from,
        state
      ) do
    new_state = prepare_connection(user, data_source_name, state)
    result = Connection.query(new_state.conn, query, params)
    {:reply, result, new_state, @idle_timeout}
  end

  @impl GenServer
  def handle_call(
        {:parse, user, data_source_name, query},
        _from,
        state
      ) do
    new_state = prepare_connection(user, data_source_name, state)
    result = Connection.parse(new_state.conn, query)
    {:reply, result, new_state, @idle_timeout}
  end

  @impl GenServer
  def handle_info(:timeout, %{last_db: nil} = state), do: {:noreply, state}

  def handle_info(:timeout, %{conn: conn}) do
    Connection.execute!(conn, "ROLLBACK")
    {:noreply, %{conn: conn, last_db: nil}}
  end

  @impl GenServer
  def terminate(_reason, %{conn: conn}) do
    if conn do
      Connection.close(conn)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp default_connection_params() do
    %{
      "host" => "127.0.0.1",
      "port" => 5432,
      "ssl" => false,
      "user" => "postgres",
      "password" => "",
      "name" => "postgres"
    }
  end

  defp prepare_connection(user, data_source_name, %{conn: conn, last_db: nil} = _state) do
    conn = ensure_connection(conn)
    Connection.execute!(conn, "BEGIN TRANSACTION")
    build_schema!(conn, user, data_source_name)
    %{conn: conn, last_db: {user, data_source_name}}
  end

  defp prepare_connection(user, data_source_name, %{last_db: {last_user, last_data_source_name}} = state)
       when user == last_user and data_source_name == last_data_source_name do
    state
  end

  defp prepare_connection(user, data_source_name, %{conn: conn} = _state) do
    Connection.execute!(conn, "ROLLBACK")
    Connection.execute!(conn, "BEGIN TRANSACTION")
    build_schema!(conn, user, data_source_name)
    %{conn: conn, last_db: {user, data_source_name}}
  end

  defp ensure_connection(nil), do: Connection.open!()
  defp ensure_connection(conn), do: conn

  # We chunk updates in batches because the operation may fail if the SQL string becomes too long.
  @chunk_size 30

  defp build_schema!(conn, user, data_source_name) do
    {time, _result} =
      :timer.tc(fn ->
        Schema.create_table_statements(user, data_source_name)
        |> Enum.chunk_every(@chunk_size)
        |> Enum.each(fn chunk ->
          sql = Enum.join(chunk, "\n")
          Connection.execute!(conn, sql)
        end)
      end)

    Logger.debug(fn -> "Shadow db creation took #{time / 1000}ms." end)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end
end
