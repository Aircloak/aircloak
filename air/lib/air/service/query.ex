defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Ecto.Changeset
  alias Air.{Repo, Schemas.DataSource, Schemas.Query, Schemas.User, Socket.Frontend.UserChannel}

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
          supervisor(Task.Supervisor, [[name: __MODULE__.TaskSupervisor]], id: __MODULE__.TaskSupervisor)
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
  @spec get_as_user(User.t, query_id, [load_result: boolean]) :: {:ok, Query.t} | {:error, :not_found | :invalid_id}
  def get_as_user(user, id, opts \\ []) do
    try do
      with {:ok, query} <- user |> query_scope() |> get(id) do
        if Keyword.get(opts, :load_result, false) do
          {:ok, Repo.preload(query, :result)}
        else
          {:ok, query}
        end
      end
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
  @spec load_recent_queries(User.t, DataSource.t, Query.Context.t, pos_integer, NaiveDateTime.t,
    [load_result: boolean]) :: [Query.t]
  def load_recent_queries(user, data_source, context, recent_count, before, opts \\ []) do
    preloads = [:user, :data_source] ++
      if Keyword.get(opts, :load_result, false), do: [:result], else: []

    Query
    |> started_by(user)
    |> for_data_source(data_source)
    |> in_context(context)
    |> recent(recent_count, before)
    |> Repo.all()
    |> Repo.preload(preloads)
    |> Enum.map(&Query.for_display/1)
  end

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
    query = Repo.get!(Query, result.query_id) |> Repo.preload([:result, :user])
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
    query = Repo.get!(Query, query_id) |> Repo.preload(:result)

    if valid_state_transition?(query.query_state, :error) do
      query = query
      |> Query.changeset(%{query_state: :error})
      |> Changeset.put_assoc(:result, Changeset.change(query.result, %{result: %{error: "Query died."}}))
      |> Repo.update!()

      UserChannel.broadcast_state_change(query)
    end

    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_task(task_fun), do:
    Task.Supervisor.start_child(__MODULE__.TaskSupervisor, task_fun)

  defp do_process_result(query, result) do
    result = decode_rows(result)

    storable_result = %{
      columns: result[:columns],
      types: result[:features][:selected_types],
      rows: result[:rows],
      error: error_text(result),
      info: result[:info],
      row_count: result.row_count || 0,
    }

    changeset =
      query
      |> Query.changeset(%{
        execution_time: result[:execution_time],
        users_count: result[:users_count],
        features: result[:features],
        query_state: query_state(result),
      })
      |> Changeset.put_assoc(:result, Changeset.change(query.result, %{result: storable_result}))

    start_task(fn -> Air.Service.Query.Events.trigger_result(result) end)
    start_task(fn ->
      query = changeset |> Changeset.apply_changes()
      UserChannel.broadcast_state_change(query)
    end)
    start_task(fn -> Repo.update!(changeset) end)

    Air.Service.Central.report_query_result(result)

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

  @state_order [
    :started, :parsing, :compiling, :awaiting_data, :ingesting_data, :processing, :post_processing, :cancelled,
    :error, :completed,
  ]
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

  defp decode_rows(%{rows: nil} = result), do:
    result
  defp decode_rows(%{rows: encoded_rows} = result) when is_binary(encoded_rows) do
    {time, decoded_rows} = :timer.tc(fn () -> :erlang.binary_to_term(encoded_rows) end)

    if time > 10_000 do
      # log processing times longer than 10ms
      Logger.warn([
        "decoding a query result for query #{result.query_id} took #{div(time, 1000)}ms, ",
        "encoded message size=#{byte_size(encoded_rows)} bytes"
      ])
    end

    %{result | rows: decoded_rows}
  end
end
