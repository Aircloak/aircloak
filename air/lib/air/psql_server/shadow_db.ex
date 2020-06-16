defmodule Air.PsqlServer.ShadowDb do
  @moduledoc """
  Service for managing shadow databases.

  A shadow database is a local PostgreSQL database which corresponds in structure to data source.
  We maintain these databases so we can offload PostgreSQL specific queries issued over the psql interface.
  """

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
    run(user, data_source_name, fn conn ->
      Connection.query(conn, query, params)
    end)
  end

  @doc "Parses the query on the given data source."
  @spec parse(User.t(), String.t(), String.t()) :: Connection.parse_result()
  def parse(user, data_source_name, query) do
    run(user, data_source_name, fn conn ->
      Connection.parse(conn, query)
    end)
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

  # We chunk updates in batches because the operation may fail if the SQL string becomes too long.
  @chunk_size 30

  defp run(user, data_source_name, fun) do
    :global.trans({__MODULE__, self()}, fn ->
      {time, result} =
        :timer.tc(fn ->
          Connection.run!(fn conn ->
            Connection.execute!(conn, "BEGIN TRANSACTION")

            try do
              build_schema!(conn, user, data_source_name)
              fun.(conn)
            after
              Connection.execute!(conn, "ROLLBACK")
            end
          end)
        end)

      Logger.debug(fn -> "Shadow db operation took #{time / 1000}ms." end)
      result
    end)
  end

  defp build_schema!(conn, user, data_source_name) do
    Schema.create_table_statements(user, data_source_name)
    |> Enum.chunk_every(@chunk_size)
    |> Enum.each(fn chunk ->
      sql = Enum.join(chunk, "\n")
      Connection.execute!(conn, sql)
    end)
  end
end
