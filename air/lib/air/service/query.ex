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
end
