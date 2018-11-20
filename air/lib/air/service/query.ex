defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Air.Repo
  alias Air.Schemas.{DataSource, Query, ResultChunk, User}
  alias Air.Service.Token
  alias AirWeb.Socket.Frontend.UserChannel
  alias AirWeb.Router

  import Ecto.Query
  require Logger

  @type query_id :: Query.id() | :autogenerate
  @type option :: {:session_id, Query.session_id()} | {:audit_meta, map}
  @type options :: [option]

  @type user_id :: non_neg_integer
  @type data_source_id :: non_neg_integer
  @type filters :: %{
          from: DateTime.t(),
          to: DateTime.t(),
          query_states: [Query.QueryState.t()],
          users: [user_id],
          data_sources: [data_source_id],
          max_results: non_neg_integer
        }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Produces a JSON blob of the query and its result for rendering"
  @spec for_display(Query.t(), nil | [map]) :: Map.t()
  def for_display(query, buckets \\ nil) do
    query = Repo.preload(query, [:user, :data_source])

    query
    |> Map.take([:id, :data_source_id, :statement, :session_id, :inserted_at, :query_state])
    |> Map.merge(query.result || %{})
    |> add_result(buckets)
    |> Map.merge(data_source_info(query))
    |> Map.merge(user_info(query))
    |> Map.put(:completed, completed?(query))
    |> Map.merge(links(query))
  end

  @doc """
  Creates and registers a query placeholder in the database. Given that it has an ID
  it can be used to attach and dispatch events to, and passed around for execution.
  """
  @spec create(
          Air.Service.DataSource.data_source_id_spec(),
          query_id,
          User.t(),
          Query.Context.t(),
          Query.statement(),
          Query.parameters(),
          options
        ) :: {:ok, Query.t()} | {:error, :unable_to_create_query | :unauthorized}
  def create(data_source_id_spec, query_id, user, context, statement, parameters, opts) do
    with {:ok, data_source} <- Air.Service.DataSource.fetch_as_user(data_source_id_spec, user) do
      if Air.Service.User.is_enabled?(user) do
        user
        |> Ecto.build_assoc(:queries)
        |> Query.changeset(%{
          statement: statement,
          parameters: %{values: parameters},
          session_id: Keyword.get(opts, :session_id),
          audit_meta: Keyword.get(opts, :audit_meta),
          data_source_id: data_source.id,
          query_state: :created,
          context: context
        })
        |> add_id_to_changeset(query_id)
        |> Repo.insert()
        |> case do
          {:ok, query} ->
            Air.Service.DataSource.QueryScheduler.notify()
            {:ok, Repo.preload(query, :user)}

          {:error, _changeset} ->
            {:error, :unable_to_create_query}
        end
      else
        {:error, :unable_to_create_query}
      end
    end
  end

  @doc "Returns queries, ordered by `inserted_at`, which have been created but not yet started on any cloak."
  @spec awaiting_start() :: [Query.t()]
  def awaiting_start() do
    from(
      q in Query,
      where: q.query_state == ^:created and is_nil(q.cloak_id),
      order_by: [asc: q.inserted_at],
      preload: [:user, :data_source]
    )
    |> Repo.all()
    |> Stream.reject(&is_nil(&1.data_source))
    |> Enum.reject(&is_nil(&1.user))
  end

  @doc """
  Returns a list of queries matching the given filters, ordered by `inserted_at`. At most `max_results` most recent
  queries will be returned.
  """
  @spec queries(filters) :: [Query.t()]
  def queries(filters) do
    Query
    |> apply_filters(filters)
    |> order_by([q], desc: q.inserted_at)
    |> limit(^filters.max_results)
    |> Repo.all()
    |> Repo.preload([:user, :data_source])
  end

  @doc "Returns a list of users of queries matching the given filters. `max_results` is ignored."
  @spec users_for_filters(filters) :: [User.t()]
  def users_for_filters(filters) do
    Query
    |> apply_filters(filters)
    |> select_users()
    |> Repo.all()
    |> include_filtered(User, filters.users)
    |> Enum.sort_by(& &1.name)
  end

  @doc "Returns a list of data sources of queries matching the given filters. `max_results` is ignored."
  @spec data_sources_for_filters(filters) :: [DataSource.t()]
  def data_sources_for_filters(filters) do
    Query
    |> apply_filters(filters)
    |> select_data_sources()
    |> Repo.all()
    |> include_filtered(DataSource, filters.data_sources)
    |> Enum.sort_by(& &1.name)
  end

  @doc "Returns a query if accessible by the given user, without associations preloaded."
  @spec get_as_user(User.t(), query_id) :: {:ok, Query.t()} | {:error, :not_found | :invalid_id}
  def get_as_user(user, id) do
    user
    |> Repo.preload([:groups])
    |> query_scope()
    |> get(id)
  end

  @doc """
  Returns the query with the given id, without associations preloaded. In most cases `get_as_user` should be used
  instead as it enforces access rules. The client of this function is responsible for enforcing any such rules.
  """
  @spec get(Ecto.Queryable.t(), query_id) :: {:ok, Query.t()} | {:error, :not_found | :invalid_id}
  def get(scope \\ Query, id) do
    case Repo.get(scope, id) do
      nil -> {:error, :not_found}
      query -> {:ok, query}
    end
  rescue
    Ecto.Query.CastError -> {:error, :invalid_id}
  end

  @doc """
  Returns the last query the given user issued, or nil if the user did not issue any queries.
  Also preloads the data source.
  """
  @spec last_for_user(User.t(), Query.Context.t()) :: Query.t() | nil
  def last_for_user(user, context) do
    Query
    |> started_by(user)
    |> in_context(context)
    |> last(:inserted_at)
    |> Repo.one()
    |> Repo.preload(:data_source)
  end

  @doc "Loads the most recent queries for a given user"
  @spec load_recent_queries(
          User.t(),
          DataSource.t(),
          Query.Context.t(),
          pos_integer,
          NaiveDateTime.t()
        ) :: [Query.t()]
  def load_recent_queries(user, data_source, context, recent_count, before),
    do:
      Query
      |> started_by(user)
      |> for_data_source(data_source)
      |> in_context(context)
      |> recent(before)
      |> limit(^recent_count)
      |> Repo.all()
      |> Repo.preload([:user, :data_source])

  @doc "Returns a list of the queries that have not yet finished."
  @spec not_finished() :: [Query.t()]
  def not_finished(), do: Repo.all(from(q in pending()))

  @doc "Returns a list of queries that have not yet finished, started by the given user on the given data source."
  @spec not_finished(User.t(), DataSource.t(), Query.Context.t()) :: [Query.t()]
  def not_finished(user, data_source, context) do
    pending()
    |> started_by(user)
    |> for_data_source(data_source)
    |> in_context(context)
    |> Repo.all()
  end

  @doc "Returns a list of the queries that have been started on cloak, but not yet finished."
  @spec started_on_cloak() :: [Query.t()]
  def started_on_cloak(), do: Repo.all(from(q in pending(), where: q.query_state != ^:created))

  @doc """
  Updates the state of the query with the given id to the given state. Only performs the update if the given state can
  occur after the current state of the query (for example "completed" after "started"). Does nothing otherwise.
  """
  @spec update_state(query_id, Query.QueryState.t()) :: :ok | {:error, :not_found | :invalid_id}
  def update_state(query_id, state) do
    with {:ok, query} <- get(query_id) do
      if __MODULE__.State.valid_state_transition?(query.query_state, state) do
        query
        |> Query.changeset(Map.merge(updated_time_spent(query), %{query_state: state}))
        |> Repo.update!()
        |> UserChannel.broadcast_state_change()
      end

      :ok
    end
  end

  @doc """
  Stores the given result sent by the cloak for the appropriate query and sets its state to "completed".
  The query id is taken from `result.query_id`.
  """
  @spec process_result(map) :: :ok
  def process_result(result) do
    query = Repo.get!(Query, result.query_id) |> Repo.preload(user: [:logins])

    if __MODULE__.State.valid_state_transition?(query.query_state, query_state(result)),
      do: do_process_result(query, result)

    Logger.info("processed result for query #{result.query_id}")
    :ok
  end

  @doc """
  Marks the query as errored due to unknown reasons. This can happen when for example the cloak goes down during
  processing and a normal error result is not received.
  """
  @spec query_died(query_id, String.t()) :: :ok
  def query_died(query_id, error) do
    query = Repo.get!(Query, query_id)

    if __MODULE__.State.valid_state_transition?(query.query_state, :error) do
      query =
        query
        |> Query.changeset(%{query_state: :error, result: %{error: error}})
        |> Repo.update!()

      UserChannel.broadcast_state_change(query)
    end

    :ok
  end

  @doc "Returns the buckets describing the desired range of rows."
  @spec buckets(Query.t(), non_neg_integer | :all) :: [map]
  def buckets(query, desired_chunk),
    do:
      query.id
      |> result_chunks(desired_chunk)
      |> Repo.all()
      |> Enum.flat_map(&ResultChunk.buckets/1)

  @doc """
  Creates a lazy stream of desired chunks.

  This function performs streaming by repeatedly issuing queries to the database.
  This approach is chosen instead of `Repo.stream/2` because `Repo.stream/2`
  requires that the database connection is open for as long as the client is
  consuming the stream, which might be a very long time.
  With this approach we don't keep the connection open, so we can safely use the
  stream regardless of the amount of chunks or the client processing time.
  """
  @spec chunks_stream(Query.t(), non_neg_integer | :all) :: Enumerable.t()
  def chunks_stream(query, :all),
    do:
      Stream.unfold(0, &{Repo.one(result_chunks(query.id, &1)), &1 + 1})
      |> Stream.take_while(&(&1 != nil))

  def chunks_stream(query, desired_chunk),
    do:
      [desired_chunk]
      |> Stream.map(&Repo.one(result_chunks(query.id, &1)))
      |> Stream.take_while(&(&1 != nil))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_process_result(query, result) do
    query = store_query_result!(query, result)
    log_result_error(query, result)
    UserChannel.broadcast_state_change(query, buckets(query, 0))
    Air.Service.Query.Events.trigger_result(result)
    report_query_result(result)
  end

  defp query_state(%{error: error}) when is_binary(error), do: :error
  defp query_state(%{cancelled: true}), do: :cancelled
  defp query_state(_), do: :completed

  defp error_text(%{error: error}) when is_binary(error), do: error
  defp error_text(%{cancelled: true}), do: "Cancelled."
  defp error_text(_), do: nil

  defp log_result_error(query, result) do
    if result[:error],
      do:
        Logger.error([
          "JSON_LOG ",
          Jason.encode_to_iodata!(%{
            type: "failed_query",
            message: result.error,
            statement: query.statement,
            data_source_id: query.data_source_id,
            user_id: query.user.id,
            user_login: Air.Service.User.main_login(query.user)
          })
        ])
  end

  defp store_query_result!(query, result) do
    # use string keys, so we end up consistent with what is returned from the database
    storable_result = %{
      "columns" => result[:columns],
      "types" => result[:features][:selected_types],
      "error" => error_text(result),
      "info" => result[:info],
      "row_count" => result.row_count || 0,
      "log" => result[:log] || ""
    }

    changeset =
      Query.changeset(
        query,
        Map.merge(updated_time_spent(query), %{
          execution_time: result[:execution_time],
          features: result[:features],
          query_state: query_state(result),
          result: storable_result
        })
      )

    Aircloak.report_long(:store_query_result, fn ->
      Repo.insert_all(
        ResultChunk,
        Enum.map(
          result.chunks,
          &%{query_id: query.id, index: &1.index, encoded_data: &1.encoded_data}
        )
      )

      Repo.update!(changeset)
    end)
  end

  defp updated_time_spent(query) do
    previous_state_change = query.last_state_change_at || query.inserted_at
    time_spent_current_state = NaiveDateTime.diff(NaiveDateTime.utc_now(), previous_state_change, :millisecond)

    %{
      last_state_change_at: NaiveDateTime.utc_now(),
      time_spent: Map.put(query.time_spent, query.query_state, time_spent_current_state)
    }
  end

  # -------------------------------------------------------------------
  # DB scopes
  # -------------------------------------------------------------------

  defp query_scope(user) do
    if User.admin?(user) do
      Query
    else
      started_by(Query, user)
    end
  end

  defp started_by(scope, user) do
    where(scope, [q], q.user_id == ^user.id)
  end

  defp pending(scope \\ Query), do: where(scope, [q], q.query_state in ^__MODULE__.State.active())

  defp apply_filters(scope, filters) do
    scope
    |> for_time(filters.from, filters.to)
    |> for_query_states(filters.query_states)
    |> for_data_source_ids(filters.data_sources)
    |> for_user_ids(filters.users)
  end

  defp for_query_states(scope, []), do: scope

  defp for_query_states(scope, query_states) do
    where(scope, [q], q.query_state in ^query_states)
  end

  defp for_data_source_ids(scope, []), do: scope

  defp for_data_source_ids(scope, data_source_ids) do
    where(scope, [q], q.data_source_id in ^data_source_ids)
  end

  defp for_user_ids(scope, []), do: scope

  defp for_user_ids(scope, user_ids) do
    where(scope, [q], q.user_id in ^user_ids)
  end

  defp for_time(scope, from, to) do
    where(scope, [q], q.inserted_at >= ^from and q.inserted_at <= ^to)
  end

  defp for_data_source(query, data_source) do
    from(q in query, where: q.data_source_id == ^data_source.id)
  end

  defp in_context(query, context) do
    from(q in query, where: q.context == ^context)
  end

  defp recent(query, before) do
    from(
      q in query,
      where: q.inserted_at < ^before,
      order_by: [desc: q.inserted_at]
    )
  end

  defp result_chunks(query_id, :all),
    do: from(chunk in ResultChunk, where: chunk.query_id == ^query_id, order_by: [asc: chunk.index])

  defp result_chunks(query_id, chunk_index),
    do: from(chunk in result_chunks(query_id, :all), where: chunk.index == ^chunk_index)

  if Mix.env() == :test do
    defp report_query_result(_), do: :ok
  else
    defp report_query_result(result), do: Air.Service.Central.report_query_result(result)
  end

  defp add_id_to_changeset(changeset, :autogenerate), do: changeset
  defp add_id_to_changeset(changeset, id), do: Query.add_id_to_changeset(changeset, id)

  # -------------------------------------------------------------------
  # Helpers for *_for_filters functions
  # -------------------------------------------------------------------

  defp select_users(query) do
    from(
      user in User,
      join: q in ^query,
      on: user.id == q.user_id,
      distinct: user.id,
      select: user
    )
  end

  defp select_data_sources(query) do
    from(
      data_source in DataSource,
      join: q in ^query,
      on: data_source.id == q.data_source_id,
      distinct: data_source.id,
      select: data_source
    )
  end

  defp include_filtered(items, schema, filtered_ids) do
    filtered_items = schema |> where([q], q.id in ^filtered_ids) |> Repo.all()
    Enum.uniq(items ++ filtered_items)
  end

  # -------------------------------------------------------------------
  # Helpers for for_display
  # -------------------------------------------------------------------

  defp data_source_info(query),
    do: %{data_source: %{name: Map.get(query.data_source || %{}, :name, "Unknown data source")}}

  defp user_info(query), do: %{user: %{name: Map.get(query.user || %{}, :name, "Unknown user")}}

  defp completed?(query), do: query.query_state in [:error, :completed, :cancelled]

  defp add_result(result, nil), do: result
  defp add_result(result, buckets), do: Map.put(result, :rows, buckets)

  defp links(query) do
    import Router.Helpers, only: [private_permalink_path: 3, public_permalink_path: 3, query_path: 3]

    %{
      private_permalink: private_permalink_path(AirWeb.Endpoint, :permalink_show, Token.private_query_token(query)),
      public_permalink: public_permalink_path(AirWeb.Endpoint, :permalink_show, Token.public_query_token(query)),
      buckets_link: query_path(AirWeb.Endpoint, :buckets, query.id)
    }
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg),
    do:
      Aircloak.ChildSpec.supervisor(
        [
          Air.Service.Query.Events,
          Air.Service.Query.Lifecycle
        ],
        strategy: :one_for_one,
        name: __MODULE__
      )
end
