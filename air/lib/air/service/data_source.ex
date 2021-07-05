defmodule Air.Service.DataSource do
  @moduledoc "Service module for working with data sources"

  alias Aircloak.ChildSpec
  alias Air.Schemas.{DataSource, Group, Query, User}
  alias Air.Repo
  alias Air.Service
  alias Air.Service.{Cloak, View, AnalystTable, Explorer}
  alias Air.Service.DataSource.QueryScheduler
  alias AirWeb.Socket.Cloak.MainChannel
  import Ecto.Query, only: [from: 2]
  import Ecto.Changeset
  require Logger

  @type data_source_map :: %{
          required(:name) => String.t(),
          required(:tables) => [table],
          optional(:errors) => [String.t()],
          optional(:database_host) => String.t()
        }

  @type data_source_id_spec :: {:id, integer} | {:name, String.t()}

  @type data_source_operation_error ::
          {:error, :expired | :unauthorized | :not_connected | :internal_error}

  @type data_source_status :: :online | :offline | :broken | :analyzing

  @type column :: %{
          :name => String.t(),
          :type => String.t(),
          :user_id => boolean,
          :comment => nil | String.t(),
          :access => String.t(),
          # Isolation status does not exist for views and analyst tables
          optional(:isolated) => boolean | String.t() | nil
        }

  @type table :: %{
          id: String.t(),
          content_type: :private | :public,
          columns: [column],
          comment: nil | String.t()
        }

  @type selectable :: %{
          id: String.t(),
          analyst_created: boolean,
          # Always nil for views and analyst tables
          content_type: :private | :public | nil,
          kind: :table | :view | :analyst_table,
          # Always false for tables
          broken: boolean,
          internal_id: String.t() | nil,
          columns: [column],
          comment: nil | String.t()
        }

  @task_supervisor __MODULE__.TaskSupervisor
  @delete_supervisor __MODULE__.DeleteSupervisor

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the count of all known non-deleted data sources."
  @spec count() :: non_neg_integer
  def count(), do: from(data_source in DataSource, select: count(data_source.id)) |> not_pending_delete() |> Repo.one!()

  @doc "Returns all known non-deleted data sources."
  @spec all() :: [DataSource.t()]
  def all(), do: DataSource |> not_pending_delete() |> Repo.all() |> Repo.preload([:groups])

  @doc "Returns all non-deleted data sources belonging to the given user."
  @spec for_user(User.t()) :: [DataSource.t()]
  def for_user(user), do: users_data_sources(user) |> not_pending_delete() |> Repo.all()

  @doc "Retrieves the data source and verifies whether it is available to the given user."
  @spec fetch_as_user(data_source_id_spec, User.t()) :: {:ok, DataSource.t()} | {:error, :unauthorized}
  def fetch_as_user(data_source_id_spec, user) do
    case Repo.one(user_data_source(user, data_source_id_spec)) do
      %DataSource{} = data_source -> {:ok, data_source}
      nil -> {:error, :unauthorized}
    end
  end

  @doc "Returns most recent queries executed on the given data source by the given user."
  @spec history(data_source_id_spec, User.t(), Query.Context.t(), pos_integer, NaiveDateTime.t()) ::
          {:ok, [Query.t()]} | {:error, :unauthorized}
  def history(data_source_id_spec, user, context, count, before) do
    with {:ok, data_source} <- fetch_as_user(data_source_id_spec, user),
         do: {:ok, Air.Service.Query.load_recent_queries(user, data_source, context, count, before)}
  end

  @doc "Returns the last query executed on the given data source by the given user."
  @spec last_query(data_source_id_spec, User.t(), Query.Context.t()) :: {:ok, Query.t() | nil} | {:error, :unauthorized}
  def last_query(data_source_id_spec, user, context) do
    with {:ok, queries} <- history(data_source_id_spec, user, context, 1, NaiveDateTime.utc_now()) do
      case queries do
        [query] -> {:ok, query}
        [] -> {:ok, nil}
      end
    end
  end

  @doc "Awaits for the query to finish, and returns it's result"
  @spec await_query(Query.t()) :: {:ok, Query.t()} | {:error, :cancelled | :query_died}
  def await_query(%Query{id: query_id} = query) do
    Service.Query.Events.subscribe(query_id)

    receive do
      {:query_state_change, %{query_id: ^query_id, state: terminal_state}}
      when terminal_state in [:query_died, :cancelled] ->
        {:error, terminal_state}

      {:query_result, %{query_id: ^query_id} = result} ->
        {:ok, query} = Air.Service.Query.get_as_user(query.user, query_id)

        {:ok,
         result
         |> Map.delete(:chunks)
         |> Map.put(:buckets, Air.Service.Query.buckets(query, :all))}
    end
  after
    Service.Query.Events.unsubscribe(query_id)
  end

  @doc """
  Stops a previously started query on the running cloak.

  This function will attempt to asynchronously stop the query on all currently connected cloaks. If the cloak where the
  query is running is not connected, the query will not be stopped on the cloak.

  This function will not change the query state. It's up to the caller to set the state of the query to the desired
  final state.
  """
  @spec stop_query_on_cloak(Query.t()) :: :ok
  def stop_query_on_cloak(query) do
    query.data_source.name
    |> Cloak.channel_pids()
    |> Enum.each(fn {channel, _cloak} ->
      Task.Supervisor.start_child(
        @task_supervisor,
        fn -> MainChannel.stop_query(channel, query.id) end,
        restart: :temporary
      )
    end)
  end

  @doc "Stops a previously started query and sets its state to `:cancelled`."
  @spec cancel_query(Query.t()) :: :ok
  def cancel_query(query) do
    query = Repo.preload(query, [:user, :data_source])
    stop_query_on_cloak(query)
    Air.Service.AuditLog.log(query.user, "Cancelled query", Query.audit_meta(query))
    Service.Query.Lifecycle.state_changed(query.id, :cancelled)
  end

  @doc "Returns a list of data sources given their names"
  @spec by_names([String.t()]) :: [DataSource.t()]
  def by_names(names \\ []), do: Repo.all(from(data_source in DataSource, where: data_source.name in ^names))

  @doc "Creates or updates a data source, returning the updated data source"
  @spec create_or_update_data_source(data_source_map()) :: DataSource.t()
  def create_or_update_data_source(data_source_map) do
    db_data = data_source_to_db_data(data_source_map)

    case Repo.get_by(DataSource, name: data_source_map.name) do
      nil -> create!(db_data)
      data_source -> update!(data_source, db_data)
    end
  end

  @doc "Whether or not a data source is available for querying. True if it has one or more cloaks online"
  @spec available?(String.t()) :: boolean
  def available?(data_source_name), do: Cloak.channel_pids(data_source_name) !== []

  @doc "Describes the current availability of the given data source."
  @spec status(DataSource.t()) :: data_source_status
  def status(data_source) do
    cond do
      not available?(data_source.name) -> :offline
      DataSource.errors(data_source) != [] -> :broken
      not DataSource.analyzed?(data_source) -> :analyzing
      true -> :online
    end
  end

  @doc "Returns tables, views, and analyst tables visible to the user"
  @spec selectables(User.t(), DataSource.t()) :: [selectable]
  def selectables(user, data_source) do
    views(user, data_source)
    |> Enum.concat(analyst_tables(user, data_source))
    |> Enum.concat(regular_tables(data_source))
    |> Enum.map(&Map.merge(%{analyst_created: false, broken: false, internal_id: nil}, &1))
  end

  @doc "Creates a data source, raises on error."
  @spec create!(map) :: DataSource.t()
  def create!(params) do
    {:ok, data_source} = create(params)
    data_source
  end

  @doc "Creates a data source."
  @spec create(map) :: {:ok, DataSource.t()} | {:error, Ecto.Changeset.t()}
  def create(params),
    do:
      %DataSource{}
      |> data_source_changeset(params)
      |> Repo.insert()

  @doc "Updates a data source, raises on error."
  @spec update!(DataSource.t(), map) :: DataSource.t()
  def update!(data_source, params) do
    {:ok, data_source} = update(data_source, params)
    data_source
  end

  @doc "Updates a data source."
  @spec update(DataSource.t(), map) :: {:ok, DataSource.t()} | {:error, Ecto.Changeset.t()}
  def update(data_source, params) do
    old_users = Repo.preload(data_source, groups: :users).groups |> Stream.flat_map(& &1.users) |> MapSet.new()

    with {:ok, data_source} <- data_source |> data_source_changeset(params) |> Repo.update() do
      new_users = Repo.preload(data_source, groups: :users).groups |> Stream.flat_map(& &1.users) |> MapSet.new()
      revoked_users = MapSet.difference(old_users, new_users)
      Enum.each(revoked_users, &Air.Service.AnalystTable.delete_all(&1, data_source))
      Explorer.data_source_updated(data_source)
      {:ok, data_source}
    end
  end

  @doc """
  Deletes the given data source in the background. The success or failure callback will be called depending on the
  result.
  """
  @spec delete!(DataSource.t(), (() -> any), (() -> any)) :: :ok
  def delete!(data_source, success_callback, failure_callback) do
    Task.Supervisor.start_child(
      @delete_supervisor,
      fn ->
        case Repo.transaction(fn -> Repo.delete!(data_source) end, timeout: :timer.hours(1)) do
          {:ok, _} ->
            success_callback.()

          {:error, _} ->
            failure_callback.()
        end
      end,
      restart: :temporary
    )

    :ok
  end

  @doc "Marks a data source as being deleted. This hides it from the web interface"
  @spec mark_as_pending_delete!(DataSource.t()) :: DataSource.t()
  def mark_as_pending_delete!(data_source),
    do:
      data_source
      |> cast(%{pending_delete: true}, [:pending_delete])
      |> Repo.update!()

  @doc "Converts data source into a changeset."
  @spec to_changeset(DataSource.t()) :: Ecto.Changeset.t()
  def to_changeset(data_source), do: data_source_changeset(data_source, %{})

  @doc "Returns the data source with the given name."
  @spec by_name(String.t()) :: DataSource.t() | nil
  def by_name(data_source_name),
    do: Repo.one(from(ds in DataSource, where: ds.name == ^data_source_name, preload: [:groups]))

  @doc "Returns the count of users per each distinct data source."
  @spec users_count() :: %{integer => non_neg_integer}
  def users_count(),
    do:
      Repo.all(
        from(
          [data_source, _group, user] in data_sources_with_groups_and_users(),
          group_by: data_source.id,
          select: {data_source.id, count(user.id, :distinct)}
        )
      )
      |> Enum.into(%{})

  @doc "Returns a unique collection of users who are permitted to use the given data source."
  @spec users(DataSource.t()) :: [User.t()]
  def users(data_source),
    do:
      Repo.all(
        from(
          [data_source, _group, user] in data_sources_with_groups_and_users(),
          where: data_source.id == ^data_source.id,
          select: user,
          distinct: true
        )
      )
      |> Repo.preload(:logins)

  @doc "Adds a data source from data source config file content."
  @spec add_preconfigured_datasource(Map.t()) ::
          {:ok, DataSource.t()} | {:error, :data_source_exists | :group_exists | :no_users}
  def add_preconfigured_datasource(%{name: name, logins: logins, group_name: group_name}) do
    with {:ok, users} <- get_users(logins),
         {:ok, group} <- add_group(group_name, users) do
      create_if_not_exists(name, group)
    end
  end

  @doc "Finds the available connected cloak and executes the provided lambda."
  @spec with_available_cloak(
          DataSource.t() | data_source_id_spec,
          User.t(),
          (%{channel_pid: pid, data_source: DataSource.t(), cloak_info: Map.t()} -> result)
        ) :: result | data_source_operation_error
        when result: :ok | {:ok, any} | {:error, any}
  def with_available_cloak(data_source_or_id, user, fun), do: do_with_available_cloak(data_source_or_id, user, fun)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp regular_tables(data_source),
    do:
      data_source
      |> DataSource.tables()
      |> Aircloak.atomize_keys()
      |> Enum.map(&Map.merge(%{kind: :table}, &1))

  defp add_group(name, users) do
    case Air.Service.Group.get_by_name(name) do
      {:ok, _group} ->
        {:error, :group_exists}

      {:error, :not_found} ->
        user_ids = Enum.map(users, & &1.id)
        params = %{name: name, admin: false, users: user_ids}
        {:ok, Air.Service.Group.create!(params)}
    end
  end

  defp get_users(logins) do
    logins
    |> Enum.map(fn login ->
      case Air.Service.User.get_by_login(login) do
        {:ok, user} -> user
        {:error, :not_found} -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
    |> case do
      [] -> {:error, :no_users}
      users -> {:ok, users}
    end
  end

  defp create_if_not_exists(name, group) do
    case by_names([name]) do
      [] -> {:ok, create!(%{name: name, tables: "[]", groups: [group.id]})}
      [_data_source] -> {:error, :data_source_exists}
    end
  end

  defp data_sources_with_groups_and_users(),
    do:
      from(
        data_source in DataSource,
        inner_join: group in assoc(data_source, :groups),
        inner_join: user in assoc(group, :users)
      )

  defp users_data_sources(user) do
    from(
      [data_source, _group, user] in data_sources_with_groups_and_users(),
      where: user.id == ^user.id,
      group_by: data_source.id,
      order_by: data_source.name
    )
  end

  defp not_pending_delete(data_sources), do: from(data_source in data_sources, where: not data_source.pending_delete)

  defp user_data_source(user, {:id, id}),
    do: from(data_source in users_data_sources(user), where: data_source.id == ^id)

  defp user_data_source(user, {:name, name}),
    do: from(data_source in users_data_sources(user), where: data_source.name == ^name)

  defp do_with_available_cloak(%DataSource{} = ds, user, fun), do: do_with_available_cloak({:id, ds.id}, user, fun)

  defp do_with_available_cloak(data_source_id_spec, user, fun) do
    with {:ok, data_source} <- fetch_as_user(data_source_id_spec, user) do
      exception_to_tuple(fn ->
        case Cloak.channel_pids(data_source.name) do
          [] ->
            {:error, :not_connected}

          channel_pids ->
            {channel_pid, cloak_info} = Enum.random(channel_pids)
            fun.(%{data_source: data_source, channel_pid: channel_pid, cloak_info: cloak_info})
        end
      end)
    end
  end

  defp exception_to_tuple(fun) do
    fun.()
  catch
    type, error ->
      Logger.error([
        "Error encountered #{inspect(type)} : #{inspect(error)}\n",
        Exception.format_stacktrace(__STACKTRACE__)
      ])

      {:error, :internal_error}
  end

  @data_source_fields ~w(
    name tables errors description pending_analysis isolated_failed
    shadow_tables_failed bounds_failed database_host supports_analyst_tables
  )a
  defp data_source_changeset(data_source, params),
    do:
      data_source
      |> cast(params, @data_source_fields)
      |> validate_required(~w(name tables)a)
      |> unique_constraint(:name)
      |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)

  defp data_source_to_db_data(data_source_map) do
    # We're pre-computing some commonly used values values here.
    # This allows us to have that data ready, without needing to decode the tables json.

    tables = data_source_map.tables
    errors = Map.get(data_source_map, :errors, [])

    %{
      name: data_source_map.name,
      tables: Jason.encode!(tables),
      errors: Jason.encode!(errors),
      database_host: data_source_map[:database_host],
      isolated_failed: filter_columns(tables, &(&1.isolated == :failed)),
      shadow_tables_failed: filter_columns(tables, &(&1.shadow_table == :failed)),
      bounds_failed: filter_columns(tables, &(&1 |> Map.get(:bounds) == :failed)),
      pending_analysis: count_columns(tables, &pending?/1) > 0,
      supports_analyst_tables: Map.get(data_source_map, :supports_analyst_tables, false)
    }
  end

  defp count_columns(tables, predicate), do: tables |> filter_columns(predicate) |> length()

  defp filter_columns(tables, predicate) do
    for table <- tables, column <- table.columns, predicate.(column) do
      "#{table.id}.#{column.name}"
    end
  end

  defp pending?(column),
    do: column[:shadow_table] == :pending or column[:isolated] == :pending or column[:bounds] == :pending

  defp views(user, data_source) do
    Enum.map(
      View.all(user, data_source),
      &%{
        content_type: nil,
        analyst_created: true,
        id: &1.name,
        comment: &1.comment,
        kind: :view,
        broken: &1.broken,
        creation_status: &1.creation_status,
        columns: &1.columns,
        internal_id: &1.id
      }
    )
  end

  defp analyst_tables(user, data_source) do
    Enum.map(
      AnalystTable.all(user, data_source),
      &%{
        content_type: nil,
        analyst_created: true,
        id: &1.name,
        comment: &1.comment,
        kind: :analyst_table,
        broken: &1.broken,
        creation_status: &1.creation_status,
        columns: &1.columns,
        internal_id: &1.id
      }
    )
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        QueryScheduler,
        ChildSpec.task_supervisor(name: @task_supervisor),
        ChildSpec.task_supervisor(name: @delete_supervisor)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end
end
