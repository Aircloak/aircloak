defmodule Air.Service.DataSource do
  @moduledoc "Service module for working with data sources"

  alias Air.{DataSource, DataSourceManager, Query, Repo, User, Socket.Cloak.MainChannel}
  import Ecto.Query, only: [from: 2]
  require Logger


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
  @spec fetch_as_user(String.t, User.t) :: {:ok, DataSource.t} | {:error, :unauthorized}
  def fetch_as_user(data_source_id, user) do
    case Repo.one(from data_source in users_data_sources(user), where: data_source.id == ^data_source_id) do
      %Air.DataSource{} = data_source -> {:ok, data_source}
      nil -> {:error, :unauthorized}
    end
  end

  @doc "Returns most recent queries executed on the given data source by the given user."
  @spec history(String.t, User.t, pos_integer) :: {:ok, [Query.t]} | {:error, :unauthorized}
  def history(data_source_id, user, count) do
    with {:ok, data_source} <- fetch_as_user(data_source_id, user), do:
      {:ok, Query.load_recent_queries(user, data_source, count)}
  end

  @doc "Returns the last query executed on the given data source by the given user."
  @spec last_query(String.t, User.t) :: {:ok, Query.t | nil} | {:error, :unauthorized}
  def last_query(data_source_id, user) do
    with {:ok, queries} <- history(data_source_id, user, 1) do
      case queries do
        [query] -> {:ok, query}
        [] -> {:ok, nil}
      end
    end
  end

  @doc "Starts the query on the given data source as the given user."
  @spec start_query(
    {:data_source_id, String.t} | {:data_source_token, String.t},
    User.t,
    String.t,
    %{atom => any}
  ) :: {:ok, Query.t} | {:error, :unauthorized | :not_connected | :internal_error | any}
  def start_query(data_source_id_or_token, user, statement, audit_meta \\ %{}) do
    with data_source_id <- data_source_id(data_source_id_or_token),
         {:ok, _data_source} <- fetch_as_user(data_source_id, user),
         query <- create_query(data_source_id, user, statement)
    do
      Air.Service.AuditLog.log(user, "Executed query",
        Map.merge(audit_meta, %{query: statement, data_source: data_source_id}))

      try do
        case DataSourceManager.channel_pids(query.data_source.global_id) do
          [channel_pid | _] ->
            with :ok <- MainChannel.run_query(channel_pid, Query.to_cloak_query(query)), do:
              {:ok, query}

          [] -> {:error, :not_connected}
        end
      catch type, error ->
        Logger.error([
          "Error running a query: #{inspect(type)}:#{inspect(error)}\n",
          Exception.format_stacktrace(System.stacktrace())
        ])

        {:error, :internal_error}
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
        for channel <- DataSourceManager.channel_pids(query.data_source.global_id), do:
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


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp users_data_sources(user) do
    from data_source in Air.DataSource,
      inner_join: group in assoc(data_source, :groups),
      inner_join: user in assoc(group, :users),
      where: user.id == ^user.id
  end

  defp data_source_id({:data_source_id, id}), do: id
  defp data_source_id({:data_source_token, token}), do: Repo.get_by!(DataSource, global_id: token)

  defp create_query(data_source_id, user, statement) do
    user
    |> Ecto.build_assoc(:queries)
    |> Query.changeset(%{data_source_id: data_source_id, statement: statement})
    |> Repo.insert!()
    |> Repo.preload(:data_source)
  end
end
