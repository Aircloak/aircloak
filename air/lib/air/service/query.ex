defmodule Air.Service.Query do
  @moduledoc "Services for retrieving queries."

  alias Air.Repo
  alias Air.Schemas.Query


  #-----------------------------------------------------------------------------------------------------------
  # API functions
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns a query with associations preloaded"
  @spec get(String.t) :: {:ok, Query.t} | {:error, :not_found}
  def get(id) do
    case Repo.get(Query, id) do
      nil -> {:error, :not_found}
      query ->
        query = Repo.preload(query, [:user, :data_source])
        {:ok, query}
    end
  end
end
