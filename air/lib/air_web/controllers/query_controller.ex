defmodule AirWeb.QueryController do
  @moduledoc false
  use Air.Web, :controller
  use Timex

  require Logger
  alias Air.{Schemas.Query, Service.DataSource}
  alias Plug.Conn.Status


  # -------------------------------------------------------------------
  # AirWeb.VerifyPermissions callback
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
    with {:ok, query} <- create_query(conn, params) do
      DataSource.start_query(
        query,
        data_source_id_spec(params),
        audit_meta: audit_log_meta(conn),
        session_id: Map.get(params, "session_id")
      )
    end
    |> case do
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

        send_buckets_as_json(conn, query, desired_chunk)

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
            send_chunks(conn, "text/csv", 200, Query.to_csv_stream(query, buckets_stream(query)))
          _ ->
            send_query_as_json(conn, query)
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

  defp buckets_stream(%Query{result: %{"rows" => _buckets}} = query), do:
    # old style result (<= 2017.3), so we can't stream
    Air.Service.Query.buckets(query, :all)
  defp buckets_stream(query), do:
    # new style result -> we can stream from database
    query
    |> Air.Service.Query.chunks_stream(:all)
    |> Stream.flat_map(&Air.Schemas.ResultChunk.buckets/1)

  defp send_query_as_json(conn, %Query{result: %{"rows" => _buckets}} = query), do:
    # old style result (<= 2017.3), so we can't stream
    json(conn, %{query: Query.for_display(query, Air.Service.Query.buckets(query, :all))})
  defp send_query_as_json(conn, query) do
    # new style result -> compute json in streaming fashion and send chunked response
    json_without_rows =
      query
      |> Query.for_display()
      |> Map.put(:columns, query.result["columns"])
      |> Poison.encode!()

    prefix_size = byte_size(json_without_rows) - 1
    << json_prefix :: binary-size(prefix_size), ?} >> = json_without_rows

    send_chunks(conn, "application/json", 200,
      Stream.concat([
        [~s({"query":)],
        [json_prefix],
        [~s(,"rows":)],
        buckets_json_chunks(query, :all),
        ["}}"]
      ])
    )
  end

  defp send_buckets_as_json(conn, %Query{result: %{"rows" => _buckets}} = query, desired_chunk), do:
    # old style result (<= 2017.3), so we can't stream
    json(conn, Air.Service.Query.buckets(query, desired_chunk))
  defp send_buckets_as_json(conn, query, desired_chunk), do:
    # new style result -> compute json in streaming fashion and send chunked response
    send_chunks(conn, "application/json", 200, buckets_json_chunks(query, desired_chunk))

  defp buckets_json_chunks(query, desired_chunks), do:
    Stream.concat([
      ["["],
        query
        |> Air.Service.Query.chunks_stream(desired_chunks)
        |> Stream.map(&drop_brackets/1)
        |> intersperse(?,),
      ["]"]
    ])

  defp drop_brackets(chunk) do
    json = Air.Schemas.ResultChunk.buckets_json(chunk)
    inner_size = byte_size(json) - 2
    << ?[, inner::binary-size(inner_size), ?] >> = json
    inner
  end

  defp intersperse(enumerable, intersperse_element), do:
    enumerable
    |> Stream.with_index()
    |> Stream.map(
      fn
        {element, 0} -> element
        {element, _} -> [intersperse_element, element]
      end
    )

  def send_chunks(conn, content_type, status, chunks_stream), do:
    Enum.reduce(chunks_stream, start_chunked_response(conn, content_type, status), &send_chunk!(&2, &1))

  defp start_chunked_response(conn, content_type, status), do:
    conn
    # We need to clear flash, since it otherwise might try to change the session, and this doesn't work with a chunked
    # response.
    |> clear_flash()
    |> put_resp_content_type(content_type)
    |> send_chunked(status)

  defp send_chunk!(conn, chunk) do
    {:ok, conn} = chunk(conn, chunk)
    conn
  end

  defp create_query(conn, params) do
    Air.Service.Query.create(
      Map.get(params, "id", :autogenerate),
      conn.assigns.current_user,
      conn.private.context,
      Map.fetch!(params, "statement"),
      _parameters = [],
      session_id: Map.get(params, "session_id")
    )
  end
end
