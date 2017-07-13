defmodule Air.QueryController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  require Logger
  alias Air.{Schemas.Query, Service.DataSource}
  alias Plug.Conn.Status


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      user: [:cancel, :create, :show, :load_history, :buckets],
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
      conn.private.context,
      Map.fetch!(params, "statement"),
      _parameters = [],
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
    case DataSource.history(data_source_id_spec(params), conn.assigns.current_user, :http, 10, before) do
      {:ok, queries} ->
        json(conn,
          Enum.map(queries, &Query.for_display(&1, Air.Service.Query.buckets(&1, 0)))
        )
      _ ->
        send_resp(conn, Status.code(:unauthorized), "Unauthorized to query data source")
    end
  end

  def buckets(conn, params) do
    case Air.Service.Query.get_as_user(conn.assigns.current_user, Map.fetch!(params, "id")) do
      {:ok, query} ->
        desired_chunk =
          case Map.fetch!(params, "chunk") do
            "all" -> :all
            other -> String.to_integer(other)
          end
        json(conn, Air.Service.Query.buckets(query, desired_chunk))

      _ ->
        send_resp(conn, Status.code(:not_found), "Query not found")
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
            csv_stream = Query.to_csv_stream(query, Air.Service.Query.buckets(query, :all))
            Enum.reduce(csv_stream, conn, fn(data, conn) ->
              {:ok, conn} = chunk(conn, data)
              conn
            end)
          _ ->
            json(conn, %{query: Query.for_display(query, Air.Service.Query.buckets(query, :all))})
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

  defp data_source_id_spec(%{"data_source_name" => name}), do: {:name, name}
  defp data_source_id_spec(%{"data_source_id" => id}), do: {:id, id}
  defp data_source_id_spec(%{"data_source_token" => global_id}), do: {:global_id, global_id}

  defp query_error(conn, :unauthorized), do:
    send_resp(conn, Status.code(:unauthorized), "Unauthorized to query data source")
  defp query_error(conn, :not_connected), do:
    send_resp(conn, Status.code(:service_unavailable), "No cloak is available for the given data source")
  defp query_error(conn, :timeout), do:
    send_resp(conn, Status.code(:gateway_timeout), "The cloak connection timed out")
  defp query_error(conn, :internal_error), do:
    send_resp(conn, Status.code(:internal_server_error), "")
  defp query_error(conn, other_error) do
    Logger.error(fn -> "Query start error: #{other_error}" end)
    json(conn, %{success: false, reason: other_error})
  end
end
