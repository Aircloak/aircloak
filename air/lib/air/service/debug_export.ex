defmodule Air.Service.DebugExport do
  @moduledoc "Service for creating an export of queries with artefacts for debugging purposes"

  alias Air.Service.{Query, View}
  alias Air.Schema.User


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates a zip-file with data related to an executed query useful for debugging purposes"
  @spec export(User.t, Query.id) :: {:ok, {charlist, binary}} | {:error, any}
  def export(user, query_id) do
    {:ok, query} = Query.get_as_user(user, query_id)
    query = Air.Repo.preload(query, [:data_source])

    files = [
      {'query.sql', query.statement},
      {'result.json', Query.buckets(query, :all) |> Poison.encode!(pretty: true)},
      {'views.json', View.user_views_map(user, query.data_source.id) |> Poison.encode!(pretty: true)},
      {'tables.json', query.data_source.tables},
    ]

    :zip.create('query-dump-#{query_id}.zip', files, [:memory])
  end
end
