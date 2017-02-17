defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Air.Repo
  alias Air.Schemas.Query

  import Ecto.Query, only: [from: 2]


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
    Repo.all(from query in Query, where: not query.completed)
  end

  @doc "Returns queries completed within the last 10 minutes"
  @spec recently_completed() :: [Query.t]
  def recently_completed() do
    Repo.all(
      from query in Query,
      where: query.completed and
        fragment("? > now() - INTERVAL '10 minutes'", query.updated_at),
      order_by: [desc: query.inserted_at],
      limit: 10
    )
  end

  @doc "Formats the queries to be shown in the activity monitor"
  @spec format_for_activity_monitor_view([Query.t]) :: [Map.t]
  def format_for_activity_monitor_view(queries) do
    queries
    |> Repo.preload([:user, :data_source])
    |> Enum.map(fn(query) ->
      %{
        id: query.id,
        analyst_name: query.user.name,
        data_source_name: query.data_source.name,
        state: query_state(query),
      }
    end)
  end

  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp query_state(query) do
    if query.completed do
      "completed"
    else
      "started"
    end
  end
end
