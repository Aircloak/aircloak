defmodule Air.QueryController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  require Logger
  alias Air.{Schemas.Query, Repo, Service.DataSource}
  alias Plug.Conn.Status


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: [:cancel, :create, :show, :load_history],
      admin: :all
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def create(conn, %{"query" => params}) do
    case DataSource.start_query(
      data_source_id_spec(params),
      conn.assigns.current_user,
      :http,
      Map.fetch!(params, "statement"),
      [],
      audit_meta: audit_log_meta(conn),
      session_id: params["session_id"]
    ) do
      {:ok, query} -> json(conn, %{success: true, query_id: query.id})
      {:error, reason} -> query_error(conn, reason)
    end
  end

  def load_history(conn, params) do
    before = case params["before"] do
      nil -> NaiveDateTime.utc_now()
      "" -> NaiveDateTime.utc_now()
      string -> NaiveDateTime.from_iso8601!(string)
    end
    case DataSource.history(data_source_id_spec(params), conn.assigns.current_user, 10, before) do
      {:ok, queries} ->
        json(conn, queries)
      _ ->
        send_resp(conn, Status.code(:unauthorized), "Unauthorized to query data source")
    end
  end

  def show(conn, %{"id" => id_type}) do
    [id | extension] = String.split(id_type, ".", parts: 2)
    case Air.Service.Query.get_as_user(conn.assigns.current_user, id) do
      {:ok, query} ->
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
      _ ->
        conn = put_status(conn, Status.code(:not_found))
        error_text = "A query with that id does not exist"
        case extension do
          ["csv"] -> text(conn, error_text)
          _ -> json(conn, %{error: error_text})
        end
    end
  end

  def cancel(conn, %{"id" => query_id}) do
    case Air.Service.Query.get_as_user(conn.assigns.current_user, query_id) do
      {:ok, query} ->
        query
        |> Repo.preload(:data_source)
        |> DataSource.stop_query(conn.assigns.current_user, audit_log_meta(conn))
        |> case do
          :ok -> json(conn, %{success: true})
          {:error, reason} -> query_error(conn, reason)
        end
      _ ->
        send_resp(conn, Status.code(:not_found), "A query with that id does not exist")
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp data_source_id_spec(%{"data_source_id" => id}), do: {:id, id}
  defp data_source_id_spec(%{"data_source_token" => global_id}), do: {:global_id, global_id}

  defp query_error(conn, :unauthorized), do:
    send_resp(conn, Status.code(:unauthorized), "Unauthorized to query data source")
  defp query_error(conn, :not_connected), do:
    send_resp(conn, Status.code(:service_unavailable), "No cloak is available for the given data source")
  defp query_error(conn, :internal_error), do:
    send_resp(conn, Status.code(:internal_server_error), "")
  defp query_error(conn, other_error) do
    Logger.error(fn -> "Query start error: #{other_error}" end)
    json(conn, %{success: false, reason: other_error})
  end
end
