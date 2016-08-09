defmodule Air.QueryController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  require Logger
  alias Air.{DataSource, Query, Repo, Token, AuditLog}
  alias Poison, as: JSON
  alias Plug.CSRFProtection
  alias Plug.Conn.Status
  alias Air.Socket.Cloak.MainChannel


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: [:index, :create, :show, :load_history],
      admin: :all
    }
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
      csrf_token: CSRFProtection.get_csrf_token(),
      last_query: JSON.encode!(last_query),
      data_sources: JSON.encode!(DataSource.all(conn.assigns.current_user.organisation))
    )
  end

  def create(conn, %{"query" => params}) do
    {:ok, query} = build_assoc(conn.assigns.current_user, :queries)
    |> Query.changeset(parse_query_params(params))
    |> Repo.insert()

    AuditLog.log(conn, "Executed query", query: query.statement, data_source: query.data_source)

    try do
      case MainChannel.run_query(
        query.cloak_id,
        conn.assigns.current_user.organisation,
        Query.to_cloak_query(query)
      ) do
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

      send_resp(conn, Status.code(:internal_server_error), "")
    end
  end

  def load_history(conn, _params) do
    json(conn, load_recent_queries(conn.assigns.current_user, 10))
  end

  def show(conn, %{"id" => id_type}) do
    [id | extension] = String.split(id_type, ".", parts: 2)
    case find_query(conn.assigns.current_user, id) do
      %Query{} = query ->
        case extension do
          ["csv"] ->
            conn = put_resp_content_type(conn, "text/csv")
            conn = send_chunked(conn, 200)
            csv_stream = Query.to_csv_stream(query)
            Enum.reduce(csv_stream, conn, fn(data, conn) ->
              {:ok, conn} = chunk(conn, data)
              conn
            end)
          _ -> json(conn, %{query: Query.for_display(query)})
        end
      nil ->
        conn = put_status(conn, Status.code(:not_found))
        error_text = "A query with that id does not exist"
        case extension do
          ["csv"] -> text(conn, error_text)
          _ -> json(conn, %{error: error_text})
        end
    end
  end

  def failed(conn, _params) do
    render(conn, "failed.html", failed_queries: Air.Repo.all(Air.Query.failed()))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp find_query(user, id) do
    user
    |> Query.for_user
    |> Repo.get(id)
  end

  defp load_recent_queries(user, recent_count) do
    user
    |> Query.for_user
    |> Query.recent(recent_count)
    |> Repo.all
    |> Enum.map(&Query.for_display/1)
  end

  defp parse_query_params(params) do
    {cloak_id, data_source} = Token.decode_data_source_token(params["data_source_token"])
    Map.merge(params, %{"cloak_id" => cloak_id, "data_source" => data_source})
  end
end
