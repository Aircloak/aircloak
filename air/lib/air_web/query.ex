defmodule AirWeb.Query do
  @moduledoc "Web specific query transformations."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Produces a JSON blob of the query and its result for rendering"
  @spec for_display(Air.Schemas.Query.t(), buckets: [map] | nil) :: map
  def for_display(query, opts \\ []) do
    query = Air.Repo.preload(query, [:user, :data_source])

    query
    |> Map.take([:id, :data_source_id, :statement, :session_id, :inserted_at, :query_state])
    |> Map.merge(query.result || %{})
    |> add_result(Keyword.get(opts, :buckets))
    |> Map.merge(data_source_info(query))
    |> Map.merge(user_info(query))
    |> Map.put(:completed, completed?(query))
    |> Map.merge(links(query, opts))
  end

  # -------------------------------------------------------------------
  # Helpers for for_display
  # -------------------------------------------------------------------

  defp add_result(result, nil), do: result
  defp add_result(result, buckets), do: Map.put(result, :rows, buckets)

  defp data_source_info(query),
    do: %{data_source: %{name: Map.get(query.data_source || %{}, :name, "Unknown data source")}}

  defp user_info(query), do: %{user: %{name: Map.get(query.user || %{}, :name, "Unknown user")}}

  defp completed?(query), do: query.query_state in [:error, :completed, :cancelled]

  defp links(query, opts) do
    if Keyword.get(opts, :authenticated?, false) == true do
      alias Air.Service.Token
      import AirWeb.Router.Helpers, only: [private_permalink_path: 3, public_permalink_path: 3, query_path: 3]

      %{
        private_permalink: private_permalink_path(AirWeb.Endpoint, :permalink_show, Token.private_query_token(query)),
        public_permalink: public_permalink_path(AirWeb.Endpoint, :permalink_show, Token.public_query_token(query)),
        buckets_link: query_path(AirWeb.Endpoint, :buckets, query.id)
      }
    else
      %{}
    end
  end
end
