defmodule Air.QueriesController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  require Logger
  alias Air.{Query, Repo}

  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{user: :all}
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    last_query = case load_recent_queries(conn.assigns.current_user, 1) do
      [query] -> query
      _ -> nil
    end
    render(conn, "index.html",
      guardian_token: Guardian.Plug.current_token(conn),
      csrf_token: Plug.CSRFProtection.get_csrf_token(),
      last_query: Poison.encode!(last_query),
      data_sources: Poison.encode!(data_sources(conn))
    )
  end

  def create(conn, %{"query" => params}) do
    {:ok, query} = build_assoc(conn.assigns.current_user, :queries)
    |> Query.changeset(parse_query_params(params))
    |> Repo.insert()

    try do
      case Air.Socket.Cloak.MainChannel.run_query(query.cloak_id, Query.to_cloak_query(query)) do
        :ok ->
          json(conn, %{success: true, query_id: query.id})
        {:error, :not_connected} ->
          json(conn, %{success: false, reason: "the cloak is not connected"})
        {:error, reason} ->
          Logger.error(fn -> "Query start error: #{reason}" end)
          json(conn, %{success: false, reason: reason})
      end
    catch type, error ->
      # We'll make a nice error log report and return 500
      Logger.error([
        "Error running a query: #{inspect(type)}:#{inspect(error)}\n",
        Exception.format_stacktrace(System.stacktrace())
      ])

      send_resp(conn, Plug.Conn.Status.code(:internal_server_error), "")
    end
  end

  def load_history(conn, _params) do
    json(conn, load_recent_queries(conn.assigns.current_user, 10))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_recent_queries(user, recent_count) do
    user
    |> Query.for_user
    |> Query.recent(recent_count)
    |> Repo.all
    |> Enum.map(&Query.for_display/1)
  end

  defp data_sources(conn) do
      for cloak <- Air.CloakInfo.all(conn.assigns.current_user.organisation),
          data_source <- cloak.data_sources
      do
        %{
          id: data_source.id,
          display: "#{data_source.id} (#{cloak.name})",
          tables: data_source.tables,
          cloak: cloak,
          token: data_source_token(cloak.id, data_source.id)
        }
      end
  end

  defp parse_query_params(params) do
    {cloak_id, data_source} = decode_data_source_token(params["data_source_token"])
    Map.merge(params, %{"cloak_id" => cloak_id, "data_source" => data_source})
  end

  defp data_source_token(nil, nil), do: nil
  defp data_source_token(cloak_id, data_source) do
    Phoenix.Token.sign(Air.Endpoint, "data_source_token", {cloak_id, data_source})
  end

  defp decode_data_source_token(nil), do: {nil, nil}
  defp decode_data_source_token(data_source_token) do
    {:ok, {cloak_id, data_source}} = Phoenix.Token.verify(Air.Endpoint, "data_source_token", data_source_token)
    {cloak_id, data_source}
  end
end
