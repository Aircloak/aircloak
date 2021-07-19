defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Air.Repo
  alias Air.Schemas.{DataSource, Query, ResultChunk, User}
  alias AirWeb.Socket.Frontend.UserChannel

  import Ecto.Query
  import Air.Ecto.Pg
  require Logger

  @type query_id :: Query.id() | :autogenerate
  @type option :: {:session_id, Query.session_id()} | {:audit_meta, map}
  @type options :: [option]

  @type user_id :: non_neg_integer
  @type data_source_id :: non_neg_integer
  @type filters :: %{
          optional(:contexts) => [Query.Context.t()],
          optional(:phrase) => String.t(),
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

  @doc "Deletes a finished query for a user."
  @spec delete_as_user(User.t(), query_id) :: :ok | {:error, :not_found | :invalid_id | :query_running}
  def delete_as_user(user, id) do
    case get_as_user(user, id) do
      {:ok, query} ->
        if query.query_state in __MODULE__.State.completed() do
          Repo.delete!(query)
          :ok
        else
          {:error, :query_running}
        end

      error ->
        error
    end
  end

  @doc "Updates the note of a user's query."
  @spec update_note_as_user(User.t(), query_id, String.t() | nil) ::
          :ok | {:error, :not_found | :invalid_id | :database_error}
  def update_note_as_user(user, id, note) do
    case get_as_user(user, id) do
      {:ok, query} ->
        query
        |> Query.changeset(%{note: note})
        |> Repo.update()
        |> case do
          {:ok, _} -> :ok
          _ -> {:error, :database_error}
        end

      error ->
        error
    end
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

  @doc "Returns a histogram of query performance matching the filters."
  @spec performance_histogram(filters) :: [
          %{
            bucket: integer,
            count: integer,
            started: float,
            parsing: float,
            compiling: float,
            "waiting for database": float,
            "ingesting data": float,
            processing: float,
            "post-processing": float,
            completed: float,
            min: float,
            max: float
          }
        ]
  def performance_histogram(filters) do
    main_query =
      Query
      |> apply_filters(filters)

    %{max: max, count: count} =
      from(
        timings in main_query,
        select: %{
          max: max(timings.total_time),
          count: count()
        }
      )
      |> Repo.one()

    if count > 0 do
      # Implements the Sturges formula (https://en.wikipedia.org/wiki/Histogram#Sturges'_formula),
      # which is probably not ideal for the expected distribution, but is hopefully not too bad.
      bin_count = ceil(:math.log2(count)) + 1

      delta = max / bin_count

      from(
        timings in main_query,
        select: %{
          bucket: width_bucket(timings.total_time, 0, ^max, ^bin_count),
          count: count(),
          started: avg(type(timings.time_spent ~>> "started", :decimal)) / 1000,
          parsing: avg(type(timings.time_spent ~>> "parsing", :decimal)) / 1000,
          compiling: avg(type(timings.time_spent ~>> "compiling", :decimal)) / 1000,
          awaiting_data: avg(type(timings.time_spent ~>> "awaiting_data", :decimal)) / 1000,
          ingesting_data: avg(type(timings.time_spent ~>> "ingesting_data", :decimal)) / 1000,
          processing: avg(type(timings.time_spent ~>> "processing", :decimal)) / 1000,
          post_processing: avg(type(timings.time_spent ~>> "post_processing", :decimal)) / 1000,
          completed: avg(type(timings.time_spent ~>> "completed", :decimal)) / 1000
        },
        order_by: [asc: 1],
        group_by: [1]
      )
      |> Repo.all()
      |> Enum.map(&format_processing_stages/1)
      |> Enum.map(fn data ->
        Map.merge(data, %{min: (data.bucket - 1) * delta / 1000, max: data.bucket * delta / 1000})
      end)
    else
      []
    end
  end

  @doc "Returns Queries matching filters that are 'interesting' from a performance analysis perspective."
  @spec performance_interesting_queries(filters) ::
          %{
            sufficient_data: true,
            median: Query.t(),
            percentile_95: Query.t(),
            percentile_99: Query.t()
          }
          | %{sufficient_data: false}
  def performance_interesting_queries(filters) do
    fields = [
      :statement,
      :time_spent,
      :total_time,
      :user_id,
      :data_source_id,
      :inserted_at
    ]

    percentile_queries =
      from(
        timings in subquery(
          Query
          |> apply_filters(filters)
          |> select([q], map(q, ^fields))
          |> select_merge([q], %{
            percentile:
              ntile(100)
              |> over(order_by: q.total_time)
          })
        ),
        distinct: timings.percentile,
        order_by: [asc: timings.percentile, desc: timings.total_time],
        where: timings.percentile in [99, 95, 50],
        join: u in User,
        on: timings.user_id == u.id,
        join: ds in DataSource,
        on: timings.data_source_id == ds.id,
        select_merge: %{user_name: u.name, data_source_name: ds.name}
      )
      |> Repo.all()
      |> update_in([Access.all(), :time_spent], &format_processing_stages/1)

    case percentile_queries do
      # you need to have at least 100 queries to get meaningful percentiles...
      [p50, p95, p99] -> %{sufficient_data: true, median: p50, percentile_95: p95, percentile_99: p99}
      _ -> %{sufficient_data: false}
    end
  end

  @doc "Returns Queries matching filters that are 'interesting' from a performance analysis perspective."
  @spec peformance_top_10_queries(filters) :: [Query.t()]
  def peformance_top_10_queries(filters) do
    Query
    |> apply_filters(filters)
    |> limit(10)
    |> order_by(desc: :total_time)
    |> join(:inner, [q], u in User, on: q.user_id == u.id)
    |> join(:inner, [q], ds in DataSource, on: q.data_source_id == ds.id)
    |> select(
      [q],
      map(q, [
        :statement,
        :time_spent,
        :total_time,
        :user_id,
        :data_source_id,
        :inserted_at,
        :query_state
      ])
    )
    |> select_merge([q, u, ds], %{user_name: u.name, data_source_name: ds.name})
    |> Repo.all()
    |> update_in([Access.all(), :time_spent], &format_processing_stages/1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp format_processing_stages(result),
    do:
      Enum.into(result, %{}, fn {key, val} ->
        {format_processing_stage(key), val}
      end)

  defp format_processing_stage(:awaiting_data), do: :"waiting for database"
  defp format_processing_stage(:ingesting_data), do: :"ingesting data"
  defp format_processing_stage(:post_processing), do: :"post-processing"
  defp format_processing_stage("awaiting_data"), do: "waiting for database"
  defp format_processing_stage("ingesting_data"), do: "ingesting data"
  defp format_processing_stage("post_processing"), do: "post-processing"
  defp format_processing_stage(any), do: any

  defp do_process_result(query, result) do
    query = store_query_result!(query, result)
    log_result_error(query, result)
    UserChannel.broadcast_state_change(query, buckets(query, 0))
    Air.Service.Query.Events.trigger_result(result)
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
      "types" => result[:selected_types],
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
          selected_types: result[:selected_types],
          parameter_types: result[:parameter_types],
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
      time_spent: Map.put(query.time_spent, to_string(query.query_state), time_spent_current_state),
      total_time: query.total_time + time_spent_current_state
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
    |> for_contexts(filters[:contexts])
    |> for_phrase(filters[:phrase])
    |> for_query_states(filters.query_states)
    |> for_data_source_ids(filters.data_sources)
    |> for_user_ids(filters.users)
  end

  defp for_phrase(scope, nil), do: scope
  defp for_phrase(scope, ""), do: scope

  defp for_phrase(scope, phrase) do
    sanitized_phrase = Regex.replace(~r/([\\%_])/, phrase, fn _, x -> "\\#{x}" end)
    pattern = "%#{sanitized_phrase}%"
    where(scope, [q], ilike(q.note, ^pattern) or ilike(q.statement, ^pattern))
  end

  defp for_contexts(scope, nil), do: scope
  defp for_contexts(scope, []), do: scope
  defp for_contexts(scope, [context]), do: where(scope, [q], q.context == ^context)
  defp for_contexts(scope, contexts), do: where(scope, [q], q.context in ^contexts)

  defp for_query_states(scope, []), do: scope
  defp for_query_states(scope, query_states), do: where(scope, [q], q.query_state in ^query_states)

  defp for_data_source_ids(scope, []), do: scope
  defp for_data_source_ids(scope, data_source_ids), do: where(scope, [q], q.data_source_id in ^data_source_ids)

  defp for_user_ids(scope, []), do: scope
  defp for_user_ids(scope, [user_id]), do: where(scope, [q], q.user_id == ^user_id)
  defp for_user_ids(scope, user_ids), do: where(scope, [q], q.user_id in ^user_ids)

  defp for_time(scope, from, to), do: where(scope, [q], q.inserted_at >= ^from and q.inserted_at <= ^to)

  defp for_data_source(query, data_source), do: from(q in query, where: q.data_source_id == ^data_source.id)

  defp in_context(query, context), do: from(q in query, where: q.context == ^context)

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

  defp include_filtered(items, _schema, []), do: items

  defp include_filtered(items, schema, filtered_ids) do
    filtered_items = schema |> where([q], q.id in ^filtered_ids) |> Repo.all()
    Enum.uniq(items ++ filtered_items)
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
