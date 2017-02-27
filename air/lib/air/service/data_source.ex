defmodule Air.Service.DataSource do
  @moduledoc "Service module for working with data sources"

  alias Air.Schemas.{DataSource, Query, User, View}
  alias Air.{DataSourceManager, PsqlServer.Protocol, Repo, Socket.Cloak.MainChannel, Socket.Frontend.UserChannel}
  alias Air.Service.Version
  import Ecto.Query, only: [from: 2]
  require Logger

  @type data_source_id_spec :: {:id, String.t} | {:global_id, String.t}

  @type start_query_options :: [
    audit_meta: %{atom => any},
    notify: boolean,
    session_id: String.t | nil
  ]

  @type data_source_operation_error ::
    {:error, :expired, :unauthorized | :not_connected | :internal_error | any}


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns the count of all known data sources."
  @spec count() :: non_neg_integer
  def count(), do:
    Repo.one!(from data_source in DataSource, select: count(data_source.id))

  @doc "Returns all data sources belonging to the given user."
  @spec for_user(User.t) :: [DataSource.t]
  def for_user(user), do: Repo.all(users_data_sources(user))

  @doc "Retrieves the data source and verifies whether it is available to the given user."
  @spec fetch_as_user(data_source_id_spec, User.t) :: {:ok, DataSource.t} | {:error, :unauthorized}
  def fetch_as_user(data_source_id_spec, user) do
    case Repo.one(user_data_source(user, data_source_id_spec)) do
      %DataSource{} = data_source -> {:ok, data_source}
      nil -> {:error, :unauthorized}
    end
  end

  @doc "Returns most recent queries executed on the given data source by the given user."
  @spec history(data_source_id_spec, User.t, pos_integer, NaiveDateTime.t) :: {:ok, [Query.t]} | {:error, :unauthorized}
  def history(data_source_id_spec, user, count, before) do
    with {:ok, data_source} <- fetch_as_user(data_source_id_spec, user), do:
      {:ok, Query.load_recent_queries(user, data_source, count, before)}
  end

  @doc "Returns the last query executed on the given data source by the given user."
  @spec last_query(data_source_id_spec, User.t) :: {:ok, Query.t | nil} | {:error, :unauthorized}
  def last_query(data_source_id_spec, user) do
    with {:ok, queries} <- history(data_source_id_spec, user, 1, NaiveDateTime.utc_now()) do
      case queries do
        [query] -> {:ok, query}
        [] -> {:ok, nil}
      end
    end
  end

  @doc "Asks the cloak to describe the query, and returns the result."
  @spec describe_query(data_source_id_spec, User.t, String.t, [Protocol.db_value]) ::
    {:ok, map} | data_source_operation_error
  def describe_query(data_source_id_spec, user, statement, parameters) do
    on_available_cloak(data_source_id_spec, user,
      fn(_cloak, data_source, channel_pid) ->
        MainChannel.describe_query(channel_pid, %{
          statement: statement,
          data_source: data_source.global_id,
          parameters: encode_parameters(parameters),
          views: user_views_map(user)
        })
      end
    )
  end

  @doc "Validates the view on the cloak."
  @spec validate_view(data_source_id_spec, User.t, View.t) ::
    {:ok, [columns :: map]} | {:error, field :: String.t, reason :: String.t} | data_source_operation_error
  def validate_view(data_source_id_spec, user, view), do:
    on_available_cloak(data_source_id_spec, user,
      fn(_cloak, data_source, channel_pid) ->
        MainChannel.validate_view(channel_pid, %{
          data_source: data_source.global_id,
          name: view.name,
          sql: view.sql,
          views: user_views_map(user)
        })
      end
    )

  @doc "Starts the query on the given data source as the given user."
  @spec start_query(data_source_id_spec, User.t, String.t, [Protocol.db_value], start_query_options) ::
    {:ok, Query.t} | data_source_operation_error
  def start_query(data_source_id_spec, user, statement, parameters, opts \\ []) do
    opts = Keyword.merge([audit_meta: %{}, notify: false], opts)

    on_available_cloak(data_source_id_spec, user,
      fn(cloak, data_source, channel_pid) ->
        query = create_query(cloak.id, data_source.id, user, statement, parameters, opts[:session_id])

        UserChannel.broadcast_state_change(query)

        Air.Service.AuditLog.log(user, "Executed query",
          Map.merge(opts[:audit_meta], %{query: statement, data_source: data_source.id}))

        if opts[:notify] == true, do: Air.QueryEvents.subscribe(query.id)

        with :ok <- MainChannel.run_query(channel_pid, cloak_query_map(query, user, parameters)), do:
          {:ok, query}
      end
    )
  end

  @doc "Runs the query synchronously and returns its result."
  @spec run_query(data_source_id_spec, User.t, String.t, [Protocol.db_value], [audit_meta: %{atom => any}]) ::
    {:ok, map} | data_source_operation_error
  def run_query(data_source_id_spec, user, statement, parameters, opts \\ []) do
    opts = [{:notify, true} | opts]
    with {:ok, %{id: query_id}} <- start_query(data_source_id_spec, user, statement, parameters, opts) do
      receive do
        {:query_result, %{"query_id" => ^query_id} = result} ->
          Air.QueryEvents.unsubscribe(query_id)
          {:ok, result}
      end
    end
  end

  @doc "Stops a previously started query."
  @spec stop_query(Query.t, User.t, %{atom => any}) :: :ok | {:error, :internal_error | :not_connected}
  def stop_query(query, user, audit_meta \\ %{}) do
    Air.Service.AuditLog.log(user, "Stopped query",
      Map.merge(audit_meta, %{query: query.statement, data_source: query.data_source.id}))

    try do
      if DataSourceManager.available?(query.data_source.global_id) do
        for {channel, _cloak_info} <- DataSourceManager.channel_pids(query.data_source.global_id), do:
          MainChannel.stop_query(channel, query.id)
        :ok
      else
        {:error, :not_connected}
      end
    catch type, error ->
      Logger.error([
        "Error stopping query: #{inspect(type)}:#{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])
      {:error, :internal_error}
    end
  end

  @doc "Returns a list of data sources given their ids"
  @spec by_ids([non_neg_integer]) :: [DataSource.t]
  def by_ids(ids \\ []) do
    Repo.all(from data_source in DataSource,
      where: data_source.id in ^ids)
  end

  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp users_data_sources(user) do
    from data_source in DataSource,
      inner_join: group in assoc(data_source, :groups),
      inner_join: user in assoc(group, :users),
      where: user.id == ^user.id,
      group_by: data_source.id
  end

  defp user_data_source(user, {:id, id}), do:
    from data_source in users_data_sources(user), where: data_source.id == ^id
  defp user_data_source(user, {:global_id, global_id}), do:
    from data_source in users_data_sources(user), where: data_source.global_id == ^global_id

  defp create_query(cloak_id, data_source_id, user, statement, parameters, session_id) do
    user
    |> Ecto.build_assoc(:queries)
    |> Query.changeset(%{
          cloak_id: cloak_id,
          data_source_id: data_source_id,
          statement: statement,
          parameters: %{values: parameters},
          session_id: session_id,
          query_state: :started,
        })
    |> Repo.insert!()
    |> Repo.preload(:data_source)
  end

  defp on_available_cloak(data_source_id, user, fun) do
    if Version.expired?() do
      {:error, :expired}
    else
      do_on_available_cloak(data_source_id, user, fun)
    end
  end

  defp do_on_available_cloak(data_source_id_spec, user, fun) do
    with {:ok, data_source} <- fetch_as_user(data_source_id_spec, user) do
      try do
        case DataSourceManager.channel_pids(data_source.global_id) do
          [] -> {:error, :not_connected}
          channel_pids ->
            {channel_pid, cloak} = Enum.random(channel_pids)
            fun.(cloak, data_source, channel_pid)
        end
      catch type, error ->
        Logger.error([
          "Error running a cloak operation: #{inspect(type)}:#{inspect(error)}\n",
          Exception.format_stacktrace(System.stacktrace())
        ])

        {:error, :internal_error}
      end
    end
  end

  defp user_views_map(user) do
    Repo.preload(user, :views).views
    |> Enum.map(&{&1.name, &1.sql})
    |> Enum.into(%{})
  end

  defp cloak_query_map(query, user, parameters) do
    %{
      id: query.id,
      statement: query.statement,
      data_source: query.data_source.global_id,
      parameters: encode_parameters(parameters),
      views: user_views_map(user)
    }
  end

  defp encode_parameters(parameters), do:
    # JSON won't work for types such as date, time, and datetime, so we're encoding parameter array to BERT.
    Base.encode16(:erlang.term_to_binary(parameters))
end
