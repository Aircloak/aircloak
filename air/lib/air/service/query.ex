defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Air.{Repo, Schemas.Query, Socket.Frontend.UserChannel}

  import Ecto.Query, only: [from: 2]
  require Logger


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns a query without associations preloaded"
  @spec get(String.t) :: {:ok, Query.t} | {:error, :not_found | :invalid_id}
  def get(id) do
    try do
      case Repo.get(Query, id) do
        nil -> {:error, :not_found}
        query ->
          {:ok, query}
      end
    rescue Ecto.Query.CastError ->
      {:error, :invalid_id}
    end
  end

  @doc "Returns a list of the queries that are currently executing"
  @spec currently_running() :: [Query.t]
  def currently_running() do
    Repo.all(from query in Query, where: query.query_state != "completed")
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
      error: result["error"],
      info: result["info"],
      row_count: row_count,
    }

    query
    |> Query.changeset(%{
      result: storable_result,
      execution_time: result["execution_time"],
      users_count: result["users_count"],
      features: result["features"],
      query_state: :completed,
    })
    |> Repo.update!()
    |> UserChannel.broadcast_result()

    Logger.info("processed result for query #{result["query_id"]}")
    :ok
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  @state_order [:started, :parsing, :compiling, :awaiting_data, :processing, :cancelled, :error, :completed]
  defp valid_state_transition?(current_state, next_state), do:
    Enum.find_index(@state_order, &(&1 == current_state)) < Enum.find_index(@state_order, &(&1 == next_state))
end
