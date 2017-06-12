defmodule Air.Service.DataSource do
  @moduledoc "Service module for working with data sources"

  alias Air.Schemas.{DataSource, Group, Query, User}
  alias Air.{PsqlServer.Protocol, Repo, Socket.Cloak.MainChannel, Socket.Frontend.UserChannel, QueryEvents}
  alias Air.Service.{Version, Cloak, View}
  import Ecto.Query, only: [from: 2]
  import Ecto.Changeset
  require Logger

  @type data_source_id_spec :: {:id, integer} | {:global_id, String.t} | {:name, String.t}

  @type start_query_options :: [
    audit_meta: %{atom => any},
    notify: boolean,
    session_id: String.t | nil
  ]

  @type data_source_operation_error ::
    {:error, :expired | :unauthorized | :not_connected | :internal_error | any}

  @type data_source_status :: :online | :offline | :broken


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the count of all known data sources."
  @spec count() :: non_neg_integer
  def count(), do:
    Repo.one!(from data_source in DataSource, select: count(data_source.id))

  @doc "Returns all known data sources."
  @spec all() :: [DataSource.t]
  def all(), do: Repo.all(DataSource) |> Repo.preload([:groups])

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
  @spec history(data_source_id_spec, User.t, Query.Context.t, pos_integer, NaiveDateTime.t) ::
    {:ok, [Query.t]} | {:error, :unauthorized}
  def history(data_source_id_spec, user, context, count, before) do
    with {:ok, data_source} <- fetch_as_user(data_source_id_spec, user), do:
      {:ok, Air.Service.Query.load_recent_queries(user, data_source, context, count, before)}
  end

  @doc "Returns the last query executed on the given data source by the given user."
  @spec last_query(data_source_id_spec, User.t, Query.Context.t) ::
    {:ok, Query.t | nil} | {:error, :unauthorized}
  def last_query(data_source_id_spec, user, context) do
    with {:ok, queries} <- history(data_source_id_spec, user, context, 1, NaiveDateTime.utc_now()) do
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
      fn(data_source, channel_pid, _cloak_info) ->
        MainChannel.describe_query(channel_pid, %{
          statement: statement,
          data_source: data_source.name,
          parameters: encode_parameters(parameters),
          views: View.user_views_map(user, data_source.id)
        })
      end
    )
  end

  @doc "Validates all of the given views on the cloak."
  @spec validate_views(data_source_id_spec, User.t, View.view_map) ::
    {:ok, map} | data_source_operation_error
  def validate_views(data_source_id_spec, user, view_map), do:
    on_available_cloak(data_source_id_spec, user,
      fn(data_source, channel_pid, _cloak_info) ->
        {:ok, MainChannel.validate_views(channel_pid, %{
          data_source: data_source.name,
          views: view_map
        })}
      end
    )

  @doc "Starts the query on the given data source as the given user."
  @spec start_query(data_source_id_spec, User.t, Query.Context.t, String.t, [Protocol.db_value],
    start_query_options) :: {:ok, Query.t} | data_source_operation_error
  def start_query(data_source_id_spec, user, context, statement, parameters, opts \\ []) do
    opts = Keyword.merge([audit_meta: %{}, notify: false], opts)

    on_available_cloak(data_source_id_spec, user,
      fn(data_source, channel_pid, %{id: cloak_id}) ->
        query = create_query(cloak_id, data_source.id, user, context, statement, parameters, opts[:session_id])

        UserChannel.broadcast_state_change(query)

        Air.Service.AuditLog.log(user, "Executed query",
          Map.merge(opts[:audit_meta], %{query: statement, data_source: data_source.name}))

        if opts[:notify] == true, do: Air.QueryEvents.subscribe(query.id)

        with :ok <- MainChannel.run_query(channel_pid, cloak_query_map(query, user, parameters)), do:
          {:ok, query}
      end
    )
  end

  @doc "Runs the query synchronously and returns its result."
  @spec run_query(data_source_id_spec, User.t, Query.Context.t, String.t, [Protocol.db_value],
    [audit_meta: %{atom => any}]) :: {:ok, map} | data_source_operation_error
  def run_query(data_source_id_spec, user, context, statement, parameters, opts \\ []) do
    opts = [{:notify, true} | opts]
    with {:ok, %{id: query_id}} <- start_query(data_source_id_spec, user, context, statement, parameters, opts) do
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
      Map.merge(audit_meta, %{query: query.statement, data_source: query.data_source.name}))

    exception_to_tuple(fn() ->
      if available?(query.data_source.name) do
        for {channel, _cloak} <- Cloak.channel_pids(query.data_source.name), do:
          MainChannel.stop_query(channel, query.id)
        QueryEvents.trigger_state_change(query.id, :cancelled)

        :ok
      else
        {:error, :not_connected}
      end
    end)
  end

  @doc """
  Can be used to check if the query is still being processed.

  Returns {:ok, true} if the query is still processed by any cloak. Returns {:ok, false} if it's not.
  Returns {:error, reason} if an error occured while trying to find that out.
  """
  @spec query_alive?(Query.t) :: {:ok, boolean} | {:error, any}
  def query_alive?(query) do
    exception_to_tuple(fn() ->
      if available?(query.data_source.name) do
        results = for {channel, _cloak} <- Cloak.channel_pids(query.data_source.name), do:
          MainChannel.query_alive?(channel, query.id)

        cond do
          Enum.any?(results, &match?({:ok, true}, &1)) -> {:ok, true}
          Enum.any?(results, &match?({:error, _}, &1)) -> Enum.find(results, &match?({:error, _}, &1))
          true -> {:ok, false}
        end
      else
        {:error, :not_connected}
      end
    end)
  end

  @doc "Returns a list of data sources given their names"
  @spec by_names([String.t]) :: [DataSource.t]
  def by_names(names \\ []), do:
    Repo.all(from data_source in DataSource,
      where: data_source.name in ^names)

  @doc "Creates or updates a data source, returning the updated data source"
  @spec create_or_update_data_source(String.t, String.t, Map.t, [String.t]) :: DataSource.t
  def create_or_update_data_source(name, global_id, tables, errors) do
    case Repo.get_by(DataSource, name: name) do
      nil ->
        # Deprecated: global_id is a remnant of Aircloak pre-version 17.3.0.
        # It has to remain for compatibility with older versions
        # (hidden from the sight of users) until version 18.1.0.
        #
        # This particular instance here is the upgrade path where
        # a customer already has a data source that is identified
        # by the global id. When upgrading to a version that uses
        # the name as the identifier instead, we want to reuse the
        # existing data source in order to retain data source permissions
        # and query histories.
        case Repo.get_by(DataSource, global_id: global_id) do
          nil ->
            create!(%{
              name: name,
              # Retain global_id until version 18.1.0
              global_id: global_id,
              tables: Poison.encode!(tables),
              errors: Poison.encode!(errors),
            })

          data_source -> update_data_source(data_source, name, tables, errors)
        end

      data_source -> update_data_source(data_source, name, tables, errors)
    end
  end

  @doc "Whether or not a data source is available for querying. True if it has one or more cloaks online"
  @spec available?(String.t) :: boolean
  def available?(data_source_name), do: Cloak.channel_pids(data_source_name) !== []

  @doc "Describes the current availability of the given data source."
  @spec status(DataSource.t) :: data_source_status
  def status(data_source) do
    if available?(data_source.name) do
      if DataSource.errors(data_source) != [] do
        :broken
      else
        :online
      end
    else
      :offline
    end
  end

  @doc "Creates a data source, raises on error."
  @spec create!(map) :: DataSource.t
  def create!(params) do
    {:ok, data_source} = create(params)
    data_source
  end

  @doc "Creates a data source."
  @spec create(map) :: {:ok, DataSource.t} | {:error, Ecto.Changeset.t}
  def create(params), do:
    %DataSource{}
    |> data_source_changeset(params)
    |> Repo.insert()

  @doc "Updates a data source, raises on error."
  @spec update!(DataSource.t, map) :: DataSource.t
  def update!(data_source, params) do
    {:ok, data_source} = update(data_source, params)
    data_source
  end

  @doc "Updates a data source."
  @spec update(DataSource.t, map) :: {:ok, DataSource.t} | {:error, Ecto.Changeset.t}
  def update(data_source, params), do:
    data_source
    |> data_source_changeset(params)
    |> Repo.update()

  @doc "Deletes the given data source, raises on error."
  @spec delete!(DataSource.t) :: DataSource.t
  def delete!(data_source), do:
    Repo.delete!(data_source)

  @doc "Converts data source into a changeset."
  @spec to_changeset(DataSource.t) :: Ecto.Changeset.t
  def to_changeset(data_source), do:
    data_source_changeset(data_source, %{})

  @doc "Returns the data source with the given name."
  @spec by_name(String.t) :: DataSource.t | nil
  def by_name(data_source_name), do:
    Repo.one(from ds in DataSource, where: ds.name == ^data_source_name, preload: [:groups])

  @doc "Returns the count of users per each distinct data source."
  @spec users_count() :: %{integer => non_neg_integer}
  def users_count(), do:
    Repo.all(
      from [data_source, _group, user] in data_sources_with_groups_and_users(),
        group_by: data_source.id,
        select: {data_source.id, count(user.id, :distinct)}
    )
    |> Enum.into(%{})

  @doc "Returns a unique collection of users who are permitted to use the given data source."
  @spec users(DataSource.t) :: [User.t]
  def users(data_source), do:
    Repo.all(
      from [data_source, _group, user] in data_sources_with_groups_and_users(),
        where: data_source.id == ^data_source.id,
        select: user,
        distinct: true
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp data_sources_with_groups_and_users(), do:
    from(
      data_source in DataSource,
      inner_join: group in assoc(data_source, :groups),
      inner_join: user in assoc(group, :users)
    )

  defp users_data_sources(user) do
    from [data_source, _group, user] in data_sources_with_groups_and_users(),
      where: user.id == ^user.id,
      group_by: data_source.id
  end

  defp user_data_source(user, {:id, id}), do:
    from data_source in users_data_sources(user), where: data_source.id == ^id
  defp user_data_source(user, {:global_id, global_id}), do:
    from data_source in users_data_sources(user), where: data_source.global_id == ^global_id
  defp user_data_source(user, {:name, name}), do:
    from data_source in users_data_sources(user), where: data_source.name == ^name

  defp create_query(cloak_id, data_source_id, user, context, statement, parameters, session_id) do
    user
    |> Ecto.build_assoc(:queries)
    |> Query.changeset(%{
          cloak_id: cloak_id,
          data_source_id: data_source_id,
          statement: statement,
          parameters: %{values: parameters},
          session_id: session_id,
          query_state: :started,
          context: context,
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
      exception_to_tuple(fn() ->
        case Cloak.channel_pids(data_source.name) do
          [] -> {:error, :not_connected}
          channel_pids ->
            {channel_pid, cloak_info} = Enum.random(channel_pids)
            fun.(data_source, channel_pid, cloak_info)
        end
      end)
    end
  end

  defp cloak_query_map(query, user, parameters) do
    %{
      id: query.id,
      statement: query.statement,
      data_source: query.data_source.name,
      parameters: encode_parameters(parameters),
      views: View.user_views_map(user, query.data_source.id)
    }
  end

  defp encode_parameters(parameters), do:
    # JSON won't work for types such as date, time, and datetime, so we're encoding parameter array to BERT.
    Base.encode16(:erlang.term_to_binary(parameters))

  defp exception_to_tuple(fun) do
    try do
      fun.()
    catch type, error ->
      Logger.error([
        "Error encountered #{inspect(type)} : #{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])

      {:error, :internal_error}
    end
  end

  defp update_data_source(data_source, name, tables, errors), do:
    update!(data_source, %{
      name: name,
      tables: Poison.encode!(tables),
      errors: Poison.encode!(errors),
    })

  defp data_source_changeset(data_source, params), do:
    data_source
    |> cast(params, ~w(name tables global_id errors description)a)
    |> validate_required(~w(name tables global_id)a)
    |> unique_constraint(:global_id)
    |> unique_constraint(:name)
    |> PhoenixMTM.Changeset.cast_collection(:groups, Air.Repo, Group)
end
