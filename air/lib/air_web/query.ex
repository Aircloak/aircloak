defmodule AirWeb.Query do
  @moduledoc "Web specific query transformations."

  alias Air.Service.Token
  import AirWeb.Router.Helpers, only: [private_permalink_path: 3, public_permalink_path: 3, query_path: 3]

  @type for_display_options :: [
          buckets: [map] | nil,
          authenticated?: boolean,
          permalink_token: String.t()
        ]

  @for_external_display_keys [
    :buckets_link,
    :completed,
    :data_source,
    :id,
    :inserted_at,
    :private_permalink,
    :public_permalink,
    :query_state,
    :rows,
    :statement,
    :note,
    :user,
    "columns",
    "error",
    "info",
    "row_count",
    "types"
  ]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Produces a JSON blob of the query for external sharing"
  @spec for_external_display(Air.Schemas.Query.t(), for_display_options) :: map
  def for_external_display(query, opts \\ []) do
    query
    |> for_display(opts)
    |> Map.take(@for_external_display_keys)
  end

  @doc "Produces a JSON blob of the query and its result for rendering"
  @spec for_display(Air.Schemas.Query.t(), for_display_options) :: map
  def for_display(query, opts \\ []) do
    query = Air.Repo.preload(query, [:user, :data_source])

    query
    |> Map.take([:id, :data_source_id, :statement, :note, :session_id, :inserted_at, :query_state])
    |> Map.merge(query.result || %{})
    |> add_result(Keyword.get(opts, :buckets))
    |> Map.merge(data_source_info(query))
    |> Map.merge(user_info(query))
    |> Map.put(:completed, completed?(query))
    |> Map.merge(links(query, opts))
    |> Map.drop(["log"])
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
    %{
      private_permalink: private_permalink(query, opts),
      public_permalink: public_permalink(query, opts),
      buckets_link: buckets_link(query, opts)
    }
  end

  defp private_permalink(query, opts) do
    # Permalinks are not rendered if query is accessed via a permalink to avoid privilege escalation.
    if Keyword.get(opts, :authenticated?) == true,
      do: private_permalink_path(AirWeb.Endpoint, :permalink_show, Token.private_query_token(query))
  end

  defp public_permalink(query, opts) do
    # Permalinks are not rendered if query is accessed via a permalink to avoid privilege escalation.
    if Keyword.get(opts, :authenticated?) == true,
      do: public_permalink_path(AirWeb.Endpoint, :permalink_show, Token.public_query_token(query))
  end

  defp buckets_link(query, opts) do
    cond do
      Keyword.get(opts, :authenticated?) == true -> query_path(AirWeb.Endpoint, :buckets, query.id)
      permalink_token_type(opts) == :public -> public_permalink_path(AirWeb.Endpoint, :permalink_buckets, token(opts))
      permalink_token_type(opts) == :private -> private_permalink_path(AirWeb.Endpoint, :permalink_buckets, token(opts))
      true -> nil
    end
  end

  defp permalink_token_type(opts) do
    case Keyword.fetch(opts, :permalink_token) do
      {:ok, token} -> Air.Service.Token.query_token_type!(token)
      :error -> nil
    end
  end

  defp token(opts), do: Keyword.fetch!(opts, :permalink_token)
end
