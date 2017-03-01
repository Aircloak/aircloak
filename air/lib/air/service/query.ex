defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Air.{Repo, Schemas.DataSource, Schemas.Query, Schemas.User, Socket.Frontend.UserChannel}

  import Ecto.Query
  require Logger


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

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
  @spec get_as_user(User.t, String.t) :: {:ok, Query.t} | {:error, :not_found | :invalid_id}
  def get_as_user(user, id) do
    try do
      case user |> query_scope() |> Repo.get(id) do
        nil -> {:error, :not_found}
        query -> {:ok, query}
      end
    rescue Ecto.Query.CastError ->
      {:error, :invalid_id}
    end
  end

  @doc "Returns the last query the given user issued, or nil if the user did not issue any queries."
  @spec last_for_user(User.t) :: Query.t | nil
  def last_for_user(user) do
    Query
    |> started_by(user)
    |> last(:inserted_at)
    |> Repo.one()
  end

  @doc "Loads the most recent queries for a given user"
  @spec load_recent_queries(User.t, DataSource.t, pos_integer, NaiveDateTime.t) :: [Query.t]
  def load_recent_queries(user, data_source, recent_count, before) do
    Query
    |> started_by(user)
    |> Query.for_data_source(data_source)
    |> Query.recent(recent_count, before)
    |> Repo.all()
    |> Enum.map(&Query.for_display/1)
  end

  @doc "Returns a list of the queries that are currently executing"
  @spec currently_running() :: [Query.t]
  def currently_running() do
    Repo.all(from query in Query, where: not query.query_state in ["completed", "error", "cancelled"])
  end

  @doc """
  Updates the state of the query with the given id to the given state. Only performs the update if the given state can
  occur after the current state of the query (for example "completed" after "started"). Does nothing otherwise.
  """
  @spec update_state(String.t, Query.QueryState.t) :: :ok | {:error, :not_found | :invalid_id}
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
  Stores the given result sent by the cloak for the appropriate query and sets its state to "completed". The query id
  is taken from `result["query_id"]`.
  """
  @spec process_result(map) :: :ok
  def process_result(result) do
    query = Repo.get!(Query, result["query_id"])

    row_count = (result["rows"] || []) |> Enum.map(&(&1["occurrences"])) |> Enum.sum

    storable_result = %{
      columns: result["columns"],
      types: result["features"]["selected_types"],
      rows: result["rows"],
      error: error_text(result),
      info: result["info"],
      row_count: row_count,
    }

    query = query
    |> Query.changeset(%{
      result: storable_result,
      execution_time: result["execution_time"],
      users_count: result["users_count"],
      features: result["features"],
      query_state: query_state(result),
    })
    |> Repo.update!()

    UserChannel.broadcast_result(query)
    UserChannel.broadcast_state_change(query)

    Logger.info("processed result for query #{result["query_id"]}")
    :ok
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  @state_order [:started, :parsing, :compiling, :awaiting_data, :processing, :cancelled, :error, :completed]
  defp valid_state_transition?(current_state, next_state), do:
    Enum.find_index(@state_order, &(&1 == current_state)) < Enum.find_index(@state_order, &(&1 == next_state))

  defp query_state(%{"error" => error}) when is_binary(error), do: :error
  defp query_state(%{"cancelled" => true}), do: :cancelled
  defp query_state(_), do: :completed

  defp error_text(%{"error" => error}) when is_binary(error), do: error
  defp error_text(%{"cancelled" => true}), do: "Cancelled."
  defp error_text(_), do: nil

  defp query_scope(user) do
    if User.admin?(user) do
      Query
    else
      started_by(Query, user)
    end
  end

  defp get(id) do
    case Repo.get(Query, id) do
      nil -> {:error, :not_found}
      query -> {:ok, query}
    end
  end

  defp started_by(scope, user) do
    where(scope, [q], q.user_id == ^user.id)
  end
end
