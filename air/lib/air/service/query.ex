defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Air.{Repo, Socket.Frontend.UserChannel}
  alias Air.Schemas.{DataSource, Query, ResultChunk, User}

  import Ecto.Query
  require Logger

  @type query_id :: String.t


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the supervisor specification for this service."
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    import Supervisor.Spec, warn: false

    supervisor(Supervisor,
      [
        [
          supervisor(Air.Service.Query.Events, []),
          Air.Service.Query.Lifecycle.supervisor_spec(),
          Air.Service.Query.ResultConverter.supervisor_spec(),
        ],
        [strategy: :one_for_one, name: __MODULE__]
      ],
      [id: __MODULE__]
    )
  end

  @doc """
  Returns information about failed queries in a paginated form.

  The data returned by this function will select a map with fields
  `id`, `inserted_at`, `data_source`, `user`, `statement`, and `error`.
  """
  @spec paginated_failed_queries(non_neg_integer) :: Scrivener.Page.t
  def paginated_failed_queries(page) do
    query = from q in Query,
      join: ds in assoc(q, :data_source),
      join: user in assoc(q, :user),
      select: %{
        id: q.id,
        inserted_at: q.inserted_at,
        data_source: ds.name,
        user: user.name,
        statement: q.statement,
        error: fragment("?->>'error'", q.result)
      },
      where:
        not is_nil(q.statement) and q.statement != "" and
        fragment("?->>'error' <> ''", q.result),
      order_by: [desc: q.inserted_at]

    Repo.paginate(query, page: page)
  end

  @doc "Returns a query if accessible by the given user, without associations preloaded."
  @spec get_as_user(User.t, query_id) :: {:ok, Query.t} | {:error, :not_found | :invalid_id}
  def get_as_user(user, id) do
    try do
      user
      |> Repo.preload([:groups])
      |> query_scope()
      |> get(id)
    rescue Ecto.Query.CastError ->
      {:error, :invalid_id}
    end
  end

  @doc """
  Returns the last query the given user issued, or nil if the user did not issue any queries.
  Also preloads the data source.
  """
  @spec last_for_user(User.t, Query.Context.t) :: Query.t | nil
  def last_for_user(user, context) do
    Query
    |> started_by(user)
    |> in_context(context)
    |> last(:inserted_at)
    |> Repo.one()
    |> Repo.preload(:data_source)
  end

  @doc "Loads the most recent queries for a given user"
  @spec load_recent_queries(User.t, DataSource.t, Query.Context.t, pos_integer, NaiveDateTime.t) :: [Query.t]
  def load_recent_queries(user, data_source, context, recent_count, before), do:
    Query
    |> started_by(user)
    |> for_data_source(data_source)
    |> in_context(context)
    |> recent(recent_count, before)
    |> Repo.all()
    |> Repo.preload([:user, :data_source])

  @doc "Returns a list of the queries that are currently executing in all contexts."
  @spec currently_running() :: [Query.t]
  def currently_running(), do: pending() |> Repo.all()

  @doc "Returns a list of queries that are currently executing, started by the given user on the given data source."
  @spec currently_running(User.t, DataSource.t, Query.Context.t) :: [Query.t]
  def currently_running(user, data_source, context) do
    pending()
    |> started_by(user)
    |> for_data_source(data_source)
    |> in_context(context)
    |> Repo.all()
  end

  @doc """
  Updates the state of the query with the given id to the given state. Only performs the update if the given state can
  occur after the current state of the query (for example "completed" after "started"). Does nothing otherwise.
  """
  @spec update_state(query_id, Query.QueryState.t) :: :ok | {:error, :not_found | :invalid_id}
  def update_state(query_id, state) do
    with {:ok, query} <- get(query_id) do
      if valid_state_transition?(query.query_state, state) do
        query
        |> Query.changeset(%{query_state: state})
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
    query = Repo.get!(Query, result.query_id) |> Repo.preload([:user])
    if valid_state_transition?(query.query_state, query_state(result)), do:
      do_process_result(query, result)

    Logger.info("processed result for query #{result.query_id}")
    :ok
  end

  @doc """
  Marks the query as errored due to unknown reasons. This can happen when for example the cloak goes down during
  processing and a normal error result is not received.
  """
  @spec query_died(query_id) :: :ok
  def query_died(query_id) do
    query = Repo.get!(Query, query_id)

    if valid_state_transition?(query.query_state, :error) do
      query = query
      |> Query.changeset(%{query_state: :error, result: %{error: "Query died."}})
      |> Repo.update!()

      UserChannel.broadcast_state_change(query)
    end

    :ok
  end

  @doc "Returns the buckets describing the desired range of rows."
  @spec buckets(Query.t, non_neg_integer | :all) :: [map]
  def buckets(%Query{result: %{"rows" => buckets}}, _desired_chunk), do:
    # Old style results (before 2017 Q4), where buckets are stored in the query table.
    buckets
  def buckets(query, desired_chunk), do:
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
  @spec chunks_stream(Query.t, non_neg_integer | :all) :: Enumerable.t
  def chunks_stream(query, :all), do:
    Stream.unfold(0, &{Repo.one(result_chunks(query.id, &1)), &1 + 1})
    |> Stream.take_while(&(&1 != nil))
  def chunks_stream(query, desired_chunk), do:
    [desired_chunk]
    |> Stream.map(&(Repo.one(result_chunks(query.id, &1))))
    |> Stream.take_while(&(&1 != nil))

  @doc "Deletes all queries by the given user from the database."
  @spec delete_all(User.t) :: :ok
  def delete_all(user) do
    Repo.transaction(fn () ->
      query_ids = Repo.all(from q in Query, where: q.user_id == ^user.id, select: q.id)
      Repo.delete_all(from c in ResultChunk, where: c.query_id in ^query_ids)
      Repo.delete_all(from q in Query, where: q.id in ^query_ids)
    end)
  end


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

  @state_order [
    :started, :parsing, :compiling, :awaiting_data, :ingesting_data, :processing, :post_processing, :cancelled,
    :error, :completed,
  ]
  defp valid_state_transition?(same_state, same_state), do: true
  defp valid_state_transition?(current_state, _next_state)
      when current_state in [:cancelled, :completed, :error], do: false
  defp valid_state_transition?(current_state, next_state), do:
    Enum.find_index(@state_order, &(&1 == current_state)) < Enum.find_index(@state_order, &(&1 == next_state))

  defp query_state(%{error: error}) when is_binary(error), do: :error
  defp query_state(%{cancelled: true}), do: :cancelled
  defp query_state(_), do: :completed

  defp error_text(%{error: error}) when is_binary(error), do: error
  defp error_text(%{cancelled: true}), do: "Cancelled."
  defp error_text(_), do: nil

  defp get(scope \\ Query, id) do
    case Repo.get(scope, id) do
      nil -> {:error, :not_found}
      query -> {:ok, query}
    end
  end

  defp log_result_error(query, result) do
    if result[:error], do:
      Logger.error([
        "JSON_LOG ",
        Poison.encode_to_iodata!(%{
          type: "failed_query",
          message: result.error,
          statement: query.statement,
          data_source_id: query.data_source_id,
          user_id: query.user.id,
          user_email: query.user.email
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
    }

    changeset =
      Query.changeset(query, %{
        execution_time: result[:execution_time],
        users_count: result[:users_count],
        features: result[:features],
        query_state: query_state(result),
        result: storable_result
      })

    Aircloak.report_long(:store_query_result, fn ->
      query = Repo.update!(changeset)

      Repo.insert_all(
        ResultChunk,
        Enum.map(result.chunks, &%{query_id: query.id, index: &1.index, encoded_data: &1.encoded_data})
      )

      query
    end)
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

  defp pending(scope \\ Query) do
    where(scope, [q], not q.query_state in ["completed", "error", "cancelled"])
  end

  defp for_data_source(query, data_source) do
    from q in query, where: q.data_source_id == ^data_source.id
  end

  defp in_context(query, context) do
    from q in query, where: q.context == ^context
  end

  defp recent(query, count, before) do
    from q in query,
    where: q.inserted_at < ^before,
    order_by: [desc: q.inserted_at],
    limit: ^count
  end

  defp result_chunks(query_id, :all), do:
    from(chunk in ResultChunk, where: chunk.query_id == ^query_id, order_by: [asc: chunk.index])
  defp result_chunks(query_id, chunk_index), do:
    from(chunk in result_chunks(query_id, :all), where: chunk.index == ^chunk_index)

  if Mix.env == :test do
    defp report_query_result(_), do: :ok
  else
    defp report_query_result(result), do:
      Air.Service.Central.report_query_result(result)
  end
end
