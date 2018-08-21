defmodule Air.PsqlServer do
  @moduledoc "Server for PostgreSQL protocol which allows PostgreSQL clients to query cloaks."

  alias Air.PsqlServer.{RanchServer, ConnectionRegistry, QueryExecution}
  alias Air.Service.{User, DataSource}
  require Logger
  require Aircloak.DeployConfig

  @behaviour RanchServer

  @type configuration :: %{
          require_ssl: boolean,
          certfile: String.t() | nil,
          keyfile: String.t() | nil
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the postgresql server configuration."
  @spec configuration() :: configuration
  def configuration() do
    default_config = %{require_ssl: true, certfile: nil, keyfile: nil}

    user_config =
      case Aircloak.DeployConfig.fetch("psql_server") do
        {:ok, configuration} ->
          configuration

        :error ->
          %{}
      end

    normalized_user_config =
      user_config
      |> Map.take(["require_ssl", "certfile", "keyfile"])
      |> Enum.map(fn {key, value} -> {String.to_atom(key), value} end)
      |> Enum.into(%{})

    Map.merge(default_config, normalized_user_config)
  end

  @doc "Verifies if SSL configuration is valid."
  @spec validate_ssl_config() :: :ok | {:error, String.t()}
  def validate_ssl_config() do
    [:certfile, :keyfile]
    |> Enum.map(&verify_ssl_file/1)
    |> Enum.filter(&match?({:error, _}, &1))
    |> Enum.map(fn {:error, error} -> error end)
    |> case do
      [] ->
        :ok

      errors ->
        {:error,
         to_string([
           "the system can't accept SSL connections over the PostgreSQL protocol for the following reasons: ",
           Enum.join(errors, ", ")
         ])}
    end
  end

  # -------------------------------------------------------------------
  # Air.PsqlServer.RanchServer callback functions
  # -------------------------------------------------------------------

  @impl RanchServer
  def init(conn, nil), do: {:ok, RanchServer.assign(conn, :async_jobs, %{})}

  @impl RanchServer
  def login(conn, password) do
    with data_source_name = conn.login_params["database"],
         data_source_id <- {:name, data_source_name},
         {:ok, user} <- User.login(conn.login_params["user"], password),
         {:ok, _} <- DataSource.fetch_as_user(data_source_id, user) do
      # We're not storing data source, since access permissions have to be checked on every query.
      # Otherwise, revoking permissions on a data source would have no effects on currently connected
      # cloak.
      # However, we're also checking access permissions now, so we can report error immediately.
      {:ok,
       conn
       |> RanchServer.assign(:user, user)
       |> RanchServer.assign(:data_source_id, data_source_id)
       |> QueryExecution.initialize(data_source_name)}
    else
      _ -> :error
    end
  end

  @impl RanchServer
  def run_query(conn, query, params, _max_rows), do: QueryExecution.run_query(conn, query, params)

  @impl RanchServer
  def cancel_query(conn, key_data) do
    ConnectionRegistry.cancel_query(key_data)
    conn
  end

  @impl RanchServer
  def describe_statement(conn, query, params), do: QueryExecution.describe_query(conn, query, params)

  @impl RanchServer
  def handle_message(conn, {ref, query_result}) do
    case Map.fetch(conn.assigns.async_jobs, ref) do
      :error ->
        conn

      {:ok, job_descriptor} ->
        async_jobs = Map.delete(conn.assigns.async_jobs, ref)
        conn = RanchServer.assign(conn, :async_jobs, async_jobs)
        job_descriptor.on_finished.(conn, query_result)
    end
  end

  def handle_message(conn, _message), do: conn

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp verify_ssl_file(key) do
    cond do
      Map.fetch!(configuration(), key) == nil ->
        {:error, "missing `#{key}` setting under the `psql_server` key in the `config.json` file"}

      not File.exists?(Path.join([Application.app_dir(:air, "priv"), "config", Map.fetch!(configuration(), key)])) ->
        {:error, "the file `#{Map.fetch!(configuration(), key)}` is missing"}

      true ->
        :ok
    end
  end

  defp ranch_opts(),
    do:
      Application.get_env(:air, Air.PsqlServer, [])
      |> Keyword.get(:ranch_opts, [])
      |> Keyword.merge(ssl_settings())

  defp ssl_settings() do
    case validate_ssl_config() do
      :ok ->
        [
          ssl: [
            certfile: Path.join([Application.app_dir(:air, "priv"), "config", configuration().certfile]),
            keyfile: Path.join([Application.app_dir(:air, "priv"), "config", configuration().keyfile])
          ]
        ]

      {:error, error} ->
        Logger.warn(error)
        []
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg),
    do:
      Supervisor.child_spec(
        {RanchServer,
         {
           Application.fetch_env!(:air, Air.PsqlServer)[:port],
           __MODULE__,
           nil,
           ranch_opts()
         }},
        id: __MODULE__
      )
end
