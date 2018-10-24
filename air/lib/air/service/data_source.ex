defmodule Air.Service.DataSource do
  @moduledoc "Service module for working with data sources"

  alias Aircloak.ChildSpec
  alias Air.Schemas.{DataSource, Group, Query, User}
  alias Air.{PsqlServer.Protocol, Repo}
  alias Air.Service.{License, Cloak, View}
  alias Air.Service
  alias AirWeb.Socket.{Cloak.MainChannel, Frontend.UserChannel}
  import Ecto.Query, only: [from: 2]
  import Ecto.Changeset
  require Logger

  @type data_source_id_spec :: {:id, integer} | {:name, String.t()}

  @type start_query_option ::
          {:audit_meta, %{atom => any}}
          | {:notify, boolean}
          | {:session_id, String.t() | nil}

  @type start_query_options :: [start_query_option]

  @type data_source_operation_error :: {:error, :expired | :unauthorized | :not_connected | :internal_error | any}

  @type data_source_status :: :online | :offline | :broken | :analyzing

  @type table :: %{
          id: String.t(),
          columns: [
            %{
              name: String.t(),
              type: String.t(),
              user_id: boolean,
              isolated: boolean | atom
            }
          ]
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

  @doc "Returns all data non-deleted sources belonging to the given user."
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

  @doc "Asks the cloak to describe the query, and returns the result."
  @spec describe_query(data_source_id_spec, User.t(), String.t(), [Protocol.db_value()]) ::
          {:ok, map} | data_source_operation_error
  def describe_query(data_source_id_spec, user, statement, parameters) do
    on_available_cloak(data_source_id_spec, user, fn data_source, channel_pid, _cloak_info ->
      MainChannel.describe_query(channel_pid, %{
        statement: statement,
        data_source: data_source.name,
        parameters: encode_parameters(parameters),
        views: View.user_views_map(user, data_source.id)
      })
    end)
  end

  @doc "Validates all of the given views on the cloak."
  @spec validate_views(data_source_id_spec, User.t(), View.view_map()) :: {:ok, map} | data_source_operation_error
  def validate_views(data_source_id_spec, user, view_map),
    do:
      on_available_cloak(data_source_id_spec, user, fn data_source, channel_pid, _cloak_info ->
        {:ok,
         MainChannel.validate_views(channel_pid, %{
           data_source: data_source.name,
           views: view_map
         })}
      end)

  @doc "Starts the query on the given data source as the given user."
  @spec start_query(Query.t(), data_source_id_spec, start_query_options) ::
          {:ok, Query.t()} | data_source_operation_error
  def start_query(query, data_source_id_spec, opts \\ []) do
    opts = Keyword.merge([audit_meta: %{}, notify: false], opts)

    on_available_cloak(data_source_id_spec, query.user, fn data_source, channel_pid, %{id: cloak_id} ->
      Air.Service.Cloak.Stats.record_query(cloak_id)

      query =
        Air.ProcessQueue.run(__MODULE__.Queue, fn ->
          query = add_cloak_info_to_query(query, cloak_id)
          UserChannel.broadcast_state_change(query)

          Air.Service.AuditLog.log(
            query.user,
            "Executed query",
            Map.merge(opts[:audit_meta], %{query: query.statement, data_source: data_source.name})
          )

          query
        end)

      if opts[:notify] == true, do: Service.Query.Events.subscribe(query.id)

      case MainChannel.run_query(channel_pid, cloak_query_map(query)) do
        :ok ->
          {:ok, query}

        {:error, reason} ->
          Service.Query.Events.trigger_result(%{query_id: query.id, error: start_query_error(reason)})

          if reason == :timeout,
            do: stop_query_async(query),
            else: Service.Query.Lifecycle.report_query_error(query.id, start_query_error(reason))

          {:error, reason}
      end
    end)
  end

  @doc "Runs the query synchronously and returns its result."
  @spec run_query(Query.t(), data_source_id_spec, start_query_options) :: {:ok, Query.t()} | data_source_operation_error
  def run_query(query, data_source_id_spec, opts \\ []) do
    opts = [{:notify, true} | opts]

    with {:ok, %{id: query_id}} <- start_query(query, data_source_id_spec, opts) do
      result =
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

      Service.Query.Events.unsubscribe(query_id)
      result
    end
  end

  @doc "Stops a previously started query."
  @spec stop_query(Query.t(), User.t(), %{atom => any}) :: :ok | {:error, :internal_error | :not_connected}
  def stop_query(query, user, audit_meta \\ %{}) do
    query = Repo.preload(query, :data_source)

    Air.Service.AuditLog.log(
      user,
      "Stopped query",
      Map.merge(audit_meta, %{query: query.statement, data_source: query.data_source.name})
    )

    do_stop_query(query)
  end

  @doc "Returns a list of data sources given their names"
  @spec by_names([String.t()]) :: [DataSource.t()]
  def by_names(names \\ []), do: Repo.all(from(data_source in DataSource, where: data_source.name in ^names))

  @doc "Creates or updates a data source, returning the updated data source"
  @spec create_or_update_data_source(String.t(), [table], [String.t()]) :: DataSource.t()
  def create_or_update_data_source(name, tables, errors) do
    db_data = data_source_to_db_data(name, tables, errors)

    case Repo.get_by(DataSource, name: name) do
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
      not analyzed?(data_source) -> :analyzing
      true -> :online
    end
  end

  @doc "Same as tables/1 but also includes views visible to the user"
  @spec views_and_tables(User.t(), DataSource.t()) :: [
          %{
            id: String.t(),
            view: boolean,
            # Always false for tables
            broken: boolean,
            internal_id: String.t() | nil,
            columns: [
              %{
                name: String.t(),
                type: String.t(),
                user_id: boolean
              }
            ]
          }
        ]
  def views_and_tables(user, data_source) do
    default_values = %{
      view: false,
      broken: false,
      internal_id: nil
    }

    View.all(user, data_source)
    |> Enum.map(fn view ->
      %{
        view: true,
        id: view.name,
        broken: view.broken,
        columns: Map.fetch!(view.result_info, "columns") |> Aircloak.atomize_keys(),
        internal_id: view.id
      }
    end)
    |> Enum.concat(DataSource.tables(data_source) |> Aircloak.atomize_keys())
    |> Enum.map(&Map.merge(default_values, &1))
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
  def update(data_source, params),
    do:
      data_source
      |> data_source_changeset(params)
      |> Repo.update()

  @doc """
  Deletes the given data source in the background. The success or failure callback will be called depending on the
  result.
  """
  @spec delete!(DataSource.t(), (() -> any), (() -> any)) :: :ok
  def delete!(data_source, success_callback, failure_callback) do
    Task.Supervisor.start_child(@delete_supervisor, fn ->
      case Repo.transaction(fn -> Repo.delete!(data_source) end, timeout: :timer.hours(1)) do
        {:ok, _} ->
          Air.PsqlServer.ShadowDb.drop(data_source.name)
          success_callback.()

        {:error, _} ->
          failure_callback.()
      end
    end)

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

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp stop_query_async(query), do: Task.Supervisor.start_child(@task_supervisor, fn -> do_stop_query(query) end)

  defp do_stop_query(query) do
    query = Repo.preload(query, :data_source)

    exception_to_tuple(fn ->
      if available?(query.data_source.name) do
        for {channel, _cloak} <- Cloak.channel_pids(query.data_source.name),
            do: MainChannel.stop_query(channel, query.id)

        Service.Query.Lifecycle.state_changed(query.id, :cancelled)

        :ok
      else
        {:error, :not_connected}
      end
    end)
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

  defp add_cloak_info_to_query(query, cloak_id) do
    query
    |> Query.changeset(%{cloak_id: cloak_id, query_state: :started})
    |> Repo.update!()
    |> Repo.preload(:data_source)
  end

  defp on_available_cloak(data_source_id, user, fun) do
    if not License.valid?() do
      {:error, :license_invalid}
    else
      do_on_available_cloak(data_source_id, user, fun)
    end
  end

  defp do_on_available_cloak(data_source_id_spec, user, fun) do
    with {:ok, data_source} <- fetch_as_user(data_source_id_spec, user) do
      exception_to_tuple(fn ->
        case Cloak.channel_pids(data_source.name) do
          [] ->
            {:error, :not_connected}

          channel_pids ->
            {channel_pid, cloak_info} = Enum.random(channel_pids)
            fun.(data_source, channel_pid, cloak_info)
        end
      end)
    end
  end

  defp cloak_query_map(query) do
    %{
      id: query.id,
      statement: query.statement,
      data_source: query.data_source.name,
      parameters: encode_parameters(query.parameters[:values]),
      views: View.user_views_map(query.user, query.data_source.id)
    }
  end

  defp encode_parameters(parameters),
    # JSON won't work for types such as date, time, and datetime, so we're encoding parameter array to BERT.
    do: Base.encode16(:erlang.term_to_binary(parameters))

  defp exception_to_tuple(fun) do
    fun.()
  catch
    type, error ->
      Logger.error([
        "Error encountered #{inspect(type)} : #{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])

      {:error, :internal_error}
  end

  @data_source_fields ~w(
    name tables errors description columns_count isolated_computed_count isolated_failed shadow_tables_computed_count
    shadow_tables_failed
  )a
  defp data_source_changeset(data_source, params),
    do:
      data_source
      |> cast(params, @data_source_fields)
      |> validate_required(~w(name tables)a)
      |> unique_constraint(:name)
      |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)

  defp start_query_error(:timeout), do: "The query could not be started due to a communication timeout."
  defp start_query_error(:too_many_queries), do: "Too many queries running on the cloak."

  defp data_source_to_db_data(name, tables, errors) do
    # We're computing total column count, and computed and failed counts for isolators and shadow tables, and storing
    # them directly. This allows us to have that data ready, without needing to decode the tables json. Since these
    # counts are frequently needed to determine the column status, we're precomputing them once.

    %{
      name: name,
      tables: Poison.encode!(tables),
      errors: Poison.encode!(errors),
      columns_count: count_columns(tables, fn _ -> true end),
      isolated_computed_count: count_columns(tables, &is_boolean(&1.isolated)),
      isolated_failed: filter_columns(tables, &(&1.isolated == :failed)),
      shadow_tables_computed_count: count_columns(tables, &(&1.shadow_table == :ok)),
      shadow_tables_failed: filter_columns(tables, &(&1.shadow_table == :failed))
    }
  end

  defp count_columns(tables, predicate), do: tables |> filter_columns(predicate) |> length()

  defp filter_columns(tables, predicate) do
    for table <- tables, column <- table.columns, predicate.(column) do
      "#{table.id}.#{column.name}"
    end
  end

  def analyzed?(%{isolated_computed_count: nil}), do: true
  def analyzed?(%{shadow_tables_computed_count: nil}), do: true

  def analyzed?(data_source) do
    data_source.isolated_computed_count == data_source.columns_count and
      data_source.shadow_tables_computed_count == data_source.columns_count
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.supervisor(
      [
        {Air.ProcessQueue, {__MODULE__.Queue, size: 5}},
        ChildSpec.task_supervisor(name: @task_supervisor, restart: :temporary),
        ChildSpec.task_supervisor(name: @delete_supervisor, restart: :temporary)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end
end
