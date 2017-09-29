defmodule Air.PsqlServer do
  @moduledoc "Server for PostgreSQL protocol which allows PostgreSQL clients to query cloaks."

  defmodule SpecialQueries do
    @moduledoc "Behaviour for handlers of special queries."

    @doc """
    Invoked by `Air.PsqlServer` when a query should be handled.

    The implementation should return a modified `conn` if it decides to handle
    the query, or `nil` otherwise.
    """
    @callback run_query(RanchServer.t, String.t) :: RanchServer.t | nil

    @doc """
    Invoked by `Air.PsqlServer` when a query should be described.

    The implementation should return a modified `conn` if it decides to handle
    the query, or `nil` otherwise.
    """
    @callback describe_query(RanchServer.t, String.t, [any]) :: RanchServer.t | nil
  end

  alias Air.PsqlServer.{Protocol, RanchServer, ConnectionRegistry}
  alias Air.Service.{User, DataSource, Version}
  require Logger
  require Aircloak.DeployConfig

  @behaviour RanchServer

  @type configuration :: %{require_ssl: boolean, certfile: String.t | nil, keyfile: String.t | nil}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Converts the type string returned from cloak to PostgreSql type atom."
  @spec psql_type(String.t) :: Protocol.Value.type
  def psql_type(type_string), do: psql_type_impl(type_string)

  @doc "Decodes the cloak query response."
  @spec decode_cloak_query_result({:ok, map} | DataSource.data_source_operation_error) :: Protocol.query_result
  def decode_cloak_query_result(query_response), do:
    do_decode_cloak_query_result(query_response)

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
      |> Enum.map(fn({key, value}) -> {String.to_atom(key), value} end)
      |> Enum.into(%{})

    Map.merge(default_config, normalized_user_config)
  end

  @doc "Verifies if SSL configuration is valid."
  @spec validate_ssl_config() :: :ok | {:error, String.t}
  def validate_ssl_config() do
    [:certfile, :keyfile]
    |> Enum.map(&verify_ssl_file/1)
    |> Enum.filter(&match?({:error, _}, &1))
    |> Enum.map(fn({:error, error}) -> error end)
    |> case do
      [] -> :ok
      errors ->
        {:error,
          to_string([
            "the system can't accept SSL connections over the PostgreSQL protocol for the following reasons: ",
            Enum.join(errors, ", ")
          ])
        }
    end
  end

  @doc "Asynchronously runs a cancellable query"
  @spec run_cancellable_query_on_cloak(RanchServer.t, String.t, [Protocol.db_value] | nil,
    ((RanchServer.t, any) -> any)) :: RanchServer.t
  def run_cancellable_query_on_cloak(conn, query, params, callback) do
    options = [{:notify_about_query_id, self()}]
    conn = start_async_query(conn, query, params, options, callback)
    receive do
      {:query_id, query_id} ->
        ConnectionRegistry.register_query(
          conn.assigns.key_data,
          conn.assigns.user.id,
          query_id
        )
    after :timer.seconds(3) ->
      Logger.debug("Did not receive a query ID. Query cannot be cancelled")
    end
    conn
  end


  # -------------------------------------------------------------------
  # Air.PsqlServer.RanchServer callback functions
  # -------------------------------------------------------------------

  @impl RanchServer
  def init(conn, nil), do:
    {:ok, RanchServer.assign(conn, :async_jobs, %{})}

  @impl RanchServer
  def login(conn, password) do
    with data_source_id <- {:name, conn.login_params["database"]},
         {:ok, user} <- User.login(conn.login_params["user"], password),
         {:ok, _} <- DataSource.fetch_as_user(data_source_id, user)
    do
      # We're not storing data source, since access permissions have to be checked on every query.
      # Otherwise, revoking permissions on a data source would have no effects on currently connected
      # cloak.
      # However, we're also checking access permissions now, so we can report error immediately.
      {:ok,
        conn
        |> RanchServer.assign(:user, user)
        |> RanchServer.assign(:data_source_id, data_source_id)
      }
    else
      _ -> :error
    end
  end

  @impl RanchServer
  def run_query(conn, query, params, _max_rows) do
    case run_special_query(conn, query) do
      {true, conn} ->
        conn
      false ->
        run_cancellable_query_on_cloak(conn, query, params,
          &RanchServer.query_result(&1, decode_cloak_query_result(&2)))
    end
  end

  @impl RanchServer
  def cancel_query(conn, key_data) do
    ConnectionRegistry.cancel_query(key_data)
    conn
  end

  @impl RanchServer
  def describe_statement(conn, query, params) do
    case describe_special_query(conn, query, params) do
      {true, conn} ->
        conn

      false ->
        user = conn.assigns.user
        data_source_id = conn.assigns.data_source_id
        converted_params = convert_params(params)
        run_async(
          conn,
          fn -> DataSource.describe_query(data_source_id, user, query, converted_params) end,
          fn(conn, describe_result) ->
            result =
              case decode_cloak_query_result(describe_result) do
                {:error, _} = error -> error
                parsed_response -> Keyword.take(parsed_response, [:columns, :param_types])
              end
            RanchServer.describe_result(conn, result)
          end
        )
    end
  end

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
  def handle_message(conn, _message), do:
    conn


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_async_query(conn, query, params, options, on_finished) do
    user = conn.assigns.user
    data_source_id = conn.assigns.data_source_id
    converted_params = convert_params(params)
    run_async(
      conn,
      fn -> DataSource.run_query(data_source_id, user, :psql, query, converted_params, options) end,
      on_finished
    )
  end

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

  defp run_async(conn, job_fun, on_finished) do
    task = Task.async(job_fun)
    async_jobs = Map.put(conn.assigns.async_jobs, task.ref, %{task: task, on_finished: on_finished})
    RanchServer.assign(conn, :async_jobs, async_jobs)
  end

  defp ranch_opts(), do:
    Application.get_env(:air, Air.PsqlServer, [])
    |> Keyword.get(:ranch_opts, [])
    |> Keyword.merge(ssl_settings())

  defp ssl_settings() do
    case validate_ssl_config() do
      :ok ->
        [ssl: [
          certfile: Path.join([Application.app_dir(:air, "priv"), "config", configuration().certfile]),
          keyfile: Path.join([Application.app_dir(:air, "priv"), "config", configuration().keyfile])
        ]]

      {:error, error} ->
        Logger.warn(error)
        []
    end
  end

  defp run_special_query(conn, query), do:
    handle_special_query(&(&1.run_query(conn, query)))

  defp describe_special_query(conn, query, params), do:
    handle_special_query(&(&1.describe_query(conn, query, params)))

  defp handle_special_query(handler_fun) do
    [SpecialQueries.Common, SpecialQueries.Tableau]
    |> Stream.map(handler_fun)
    |> Stream.reject(&(&1 == nil))
    |> Enum.take(1)
    |> case do
      [conn] -> {true, conn}
      [] -> false
    end
  end

  defp do_decode_cloak_query_result({:error, :cancelled}), do:
    {:error, :query_cancelled}
  defp do_decode_cloak_query_result({:error, :query_died}), do:
    {:error, {:fatal, "The query terminated unexpectedly."}}
  defp do_decode_cloak_query_result({:error, :not_connected}), do:
    {:error, "Data source is not available!"}
  defp do_decode_cloak_query_result({:error, :expired}), do:
    {:error, "Your Aircloak installation is running version #{Air.SharedView.version()} " <>
      "which expired on #{Version.expiry_date()}."}
  defp do_decode_cloak_query_result({:ok, %{error: error}}), do:
    {:error, error}
  defp do_decode_cloak_query_result({:ok, query_result}), do:
    [
      columns:
        Enum.zip(query_result.columns, query_result.features.selected_types)
        |> Enum.map(fn({name, sql_type}) -> %{name: name, type: psql_type(sql_type)} end),
      rows:
        Air.Schemas.ResultChunk.rows(Map.get(query_result, :buckets, [])),
      param_types:
        Enum.map(query_result.features.parameter_types, &psql_type/1)
    ]
  defp do_decode_cloak_query_result(other) do
    Logger.error("Error running a query: #{inspect other}")
    {:error, "System error!"}
  end

  defp convert_params(nil), do: nil
  defp convert_params(params), do:
    Enum.map(params, fn({type, value}) -> %{type: sql_type(type, value), value: value} end)


  # -------------------------------------------------------------------
  # Type conversions
  # -------------------------------------------------------------------

  for {psql_type, sql_type} <- %{
    boolean: :boolean,
    int2: :integer,
    int4: :integer,
    int8: :integer,
    float4: :real,
    float8: :real,
    numeric: :real,
    date: :date,
    time: :time,
    timestamp: :datetime,
    text: :text
  } do
    defp sql_type(unquote(psql_type), _value), do: unquote(sql_type)
  end
  defp sql_type(:unknown, value), do: sql_type_from_value(value)

  defp sql_type_from_value(value) when is_boolean(value), do: :boolean
  defp sql_type_from_value(value) when is_integer(value), do: :integer
  defp sql_type_from_value(value) when is_float(value), do: :real
  defp sql_type_from_value(value) when is_binary(value), do: :text
  defp sql_type_from_value(%Date{}), do: :date
  defp sql_type_from_value(%Time{}), do: :time
  defp sql_type_from_value(%NaiveDateTime{}), do: :datetime

  for {sql_type, psql_type} <- %{
    "boolean" => :boolean,
    "integer" => :int8,
    "real" => :float8,
    "text" => :text,
    "date" => :date,
    "time" => :time,
    "datetime" => :timestamp,
  } do
    defp psql_type_impl(unquote(sql_type)), do: unquote(psql_type)
  end
  defp psql_type_impl(_other), do: :unknown


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg), do:
    Supervisor.child_spec(
      {RanchServer, {
        Application.fetch_env!(:air, Air.PsqlServer)[:port],
        __MODULE__,
        nil,
        ranch_opts()
      }},
      id: __MODULE__
    )
end
