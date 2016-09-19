defmodule Air.QueryController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  require Logger
  alias Air.{DataSource, DataSourceManager, Query, Repo, AuditLog}
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
    conn.assigns.current_user
    |> Query.for_user()
    |> Query.last()
    |> Repo.one()
    |> case do
      %Query{data_source_id: data_source_id} when data_source_id != nil ->
        redirect(conn, to: "/data_sources/#{data_source_id}")
      _ ->
        redirect(conn, to: "/data_sources")
    end
  end

  def create(conn, %{"query" => params}) do
    query = build_assoc(conn.assigns.current_user, :queries)
    |> Query.changeset(parse_query_params(params))
    |> Repo.insert!()
    |> Repo.preload(:data_source)

    if DataSourceManager.available?(query.data_source.global_id) do
      execute_query(conn, query)
    else
      send_resp(conn, Status.code(:service_unavailable), "No cloak is available for the given data source")
    end
  end

  def load_history(conn, %{"data_source_id" => data_source_id}) do
    case Repo.get(DataSource, data_source_id) do
      nil ->
        response = %{
          success: false,
          error: "Datasource is not available. Cannot load history"
        }
        json(conn, response)
      data_source ->
        json(conn, Query.load_recent_queries(conn.assigns.current_user, data_source, 10))
    end
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
    render(conn, "failed.html", failed_queries: Repo.all(Query.failed()))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp find_query(user, id) do
    user
    |> Query.for_user()
    |> Repo.get(id)
  end

  defp parse_query_params(params) do
    data_source = case params["data_source_id"] do
      nil -> Repo.get_by!(DataSource, global_id: params["data_source_token"])
      id -> Repo.get!(DataSource, id)
    end
    Map.merge(params, %{"data_source_id" => data_source.id})
  end

  defp execute_query(conn, query) do
    AuditLog.log(conn, "Executed query", query: query.statement, data_source: query.data_source.id)

    try do
      case MainChannel.run_query(
        hd(DataSourceManager.channel_pids(query.data_source.global_id)),
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
end
