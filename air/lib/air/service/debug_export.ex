defmodule Air.Service.DebugExport do
  @moduledoc "Service for creating an export of queries with artefacts for debugging purposes"

  alias Air.Service.{Query, View}
  alias Air.Schema.User


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates a zip-file with data related to an executed query useful for debugging purposes"
  @spec assemble(User.t, Query.id) :: Map.t
  def assemble(user, query_id) do
    {:ok, query} = Query.get_as_user(user, query_id)
    query = Air.Repo.preload(query, [:data_source])

    %{
      metadata: %{
        cloak_id: query.cloak_id,
        context: query.context,
        created_on: query.inserted_at,
        email: user.email,
        features: query.features || [],
        last_update: query.updated_at,
        user: user.name,
      },
      statement: query.statement,
      views: View.user_views_map(user, query.data_source_id),
      tables: query.data_source.tables |> Poison.decode!() |> Aircloak.atomize_keys(),
      result: Query.buckets(query, :all) |> Poison.encode!(pretty: true),
    }
  end
end
