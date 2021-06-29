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
      user: [
        :buckets,
        :cancel,
        :create,
        :debug_export,
        :delete,
        :load_history,
        :show,
        :update_note
      ],
      admin: :all,
      anonymous: [:permalink_show, :permalink_buckets]
    }
  end

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def create(conn, %{"query" => params}) do
    case create_query(conn, params) do
      {:ok, query} -> json(conn, %{success: true, query_id: query.id})
      {:error, reason} -> query_error(conn, reason)
    end
  end

  def load_history(conn, params) do
    before =
      case params["before"] do
        nil -> NaiveDateTime.utc_now()
        "" -> NaiveDateTime.utc_now()
        string -> NaiveDateTime.from_iso8601!(string)
      end

    case DataSource.history(
           data_source_id_spec(params),
           conn.assigns.current_user,
           :http,
           10,
           before
         ) do
      {:ok, queries} ->
        json(
          conn,
          Enum.map(
            queries,
            &AirWeb.Query.for_display(&1,
              authenticated?: true,
              buckets: Air.Service.Query.buckets(&1, 0)
            )
          )
        )

      _ ->
        send_resp(conn, Status.code(:unauthorized), "Unauthorized to query data source")
    end
  end

  def buckets(conn, params) do
    case Air.Service.Query.get_as_user(conn.assigns.current_user, Map.fetch!(params, "id")) do
      {:ok, query} -> render_buckets(conn, params, query)
      _ -> send_resp(conn, Status.code(:not_found), "Query not found")
    end
  end

  def permalink_buckets(conn, params) do
    case Air.Service.Token.query_from_token(conn.assigns.current_user, params["token"]) do
      {:ok, query} -> render_buckets(conn, params, query)
      _ -> send_resp(conn, Status.code(:not_found), "Query not found")
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

  def permalink_show(conn, params) do
    current_user = conn.assigns.current_user

    case Air.Service.Token.query_from_token(current_user, params["token"]) do
      {:ok, query} ->
        query_for_display =
          AirWeb.Query.for_external_display(query,
            authenticated?: false,
            permalink_token: params["token"],
            buckets: Air.Service.Query.buckets(query, 0)
          )

        is_owner = !!(current_user != nil && current_user.id == query.user_id)

        render(conn, "permalink_show.html",
          query: query_for_display,
          csrf_token: Plug.CSRFProtection.get_csrf_token(),
          number_format: Air.Service.User.number_format_settings(current_user),
          is_owner: is_owner
        )

      :error ->
        not_found(conn)
    end
  end

  def cancel(conn, %{"id" => query_id}) do
    case Air.Service.Query.get_as_user(conn.assigns.current_user, query_id) do
      {:ok, query} ->
        DataSource.cancel_query(query)
        json(conn, %{success: true})

      _ ->
        send_resp(conn, Status.code(:not_found), "A query with that id does not exist")
    end
  end

  def delete(conn, %{"id" => query_id}) do
    with {:ok, query} <- Air.Service.Query.get_as_user(conn.assigns.current_user, query_id),
         query <- Repo.preload(query, :data_source),
         :ok <- Air.Service.Query.delete_as_user(conn.assigns.current_user, query_id) do
      audit_log(conn, "Deleted query",
        query_id: query_id,
        statement: query.statement,
        data_source: query.data_source.name
      )

      json(conn, %{success: true})
    else
      {:error, :query_running} ->
        send_resp(conn, Status.code(:conflict), "A running query must be cancelled first")

      {:error, reason} when reason in [:not_found, :invalid_id] ->
        send_resp(conn, Status.code(:not_found), "A query with that id does not exist")

      _ ->
        send_resp(conn, Status.code(:internal_server_error), "Failed to delete the query")
    end
  end

  def update_note(conn, %{"id" => query_id, "note" => note}) do
    case Air.Service.Query.update_note_as_user(conn.assigns.current_user, query_id, note) do
      :ok ->
        json(conn, %{success: true})

      {:error, reason} when reason in [:not_found, :invalid_id] ->
        send_resp(conn, Status.code(:not_found), "A query with that id does not exist")

      _ ->
        send_resp(conn, Status.code(:internal_server_error), "Failed to update query note")
    end
  end

  def debug_export(conn, %{"id" => query_id}) do
    debug_file_content =
      Phoenix.View.render_to_iodata(
        AirWeb.QueryView,
        "debug_export.txt",
        data: Air.Service.DebugExport.assemble(conn.assigns.current_user, query_id)
      )

    conn
    |> put_resp_content_type("text/plain")
    |> put_resp_header(
      "content-disposition",
      ~s[attachment; filename="debug_export_#{query_id}.txt"]
    )
    |> send_resp(200, debug_file_content)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp data_source_id_spec(%{"data_source_name" => name}), do: {:name, name}
  defp data_source_id_spec(%{"data_source_id" => id}), do: {:id, id}

  defp query_error(conn, :unauthorized),
    do: send_resp(conn, Status.code(:unauthorized), "Unauthorized to query data source")

  defp query_error(conn, other_error) do
    Logger.error(fn -> "Query start error: #{other_error}" end)
    json(conn, %{success: false, reason: other_error})
  end

  defp buckets_stream(query),
    # new style result -> we can stream from database
    do:
      query
      |> Air.Service.Query.chunks_stream(:all)
      |> Stream.flat_map(&Air.Schemas.ResultChunk.buckets/1)

  defp send_query_as_json(conn, query) do
    # new style result -> compute json in streaming fashion and send chunked response
    json_without_rows =
      query
      |> AirWeb.Query.for_external_display(authenticated?: true)
      |> Jason.encode!()

    prefix_size = byte_size(json_without_rows) - 1
    <<json_prefix::binary-size(prefix_size), ?}>> = json_without_rows

    send_chunks(
      conn,
      "application/json",
      200,
      Stream.concat([
        [~s({"query":)],
        [json_prefix],
        [~s(,"rows":)],
        buckets_json_chunks(query, :all),
        ["}}"]
      ])
    )
  end

  defp render_buckets(conn, params, query) do
    desired_chunk =
      case Map.fetch!(params, "chunk") do
        "all" -> :all
        other -> String.to_integer(other)
      end

    send_buckets_as_json(conn, query, desired_chunk)
  end

  defp send_buckets_as_json(conn, %Query{result: %{"rows" => _buckets}} = query, desired_chunk),
    # old style result (<= 2017.3), so we can't stream
    do: json(conn, Air.Service.Query.buckets(query, desired_chunk))

  defp send_buckets_as_json(conn, query, desired_chunk),
    # new style result -> compute json in streaming fashion and send chunked response
    do: send_chunks(conn, "application/json", 200, buckets_json_chunks(query, desired_chunk))

  defp buckets_json_chunks(query, desired_chunks),
    do:
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
    <<?[, inner::binary-size(inner_size), ?]>> = json
    inner
  end

  defp intersperse(enumerable, intersperse_element),
    do:
      enumerable
      |> Stream.with_index()
      |> Stream.map(fn
        {element, 0} -> element
        {element, _} -> [intersperse_element, element]
      end)

  def send_chunks(conn, content_type, status, chunks_stream),
    do:
      Enum.reduce(
        chunks_stream,
        start_chunked_response(conn, content_type, status),
        &send_chunk!(&2, &1)
      )

  defp start_chunked_response(conn, content_type, status),
    do:
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
      data_source_id_spec(params),
      Map.get(params, "id", :autogenerate),
      conn.assigns.current_user,
      conn.private.context,
      Map.fetch!(params, "statement"),
      _parameters = [],
      session_id: Map.get(params, "session_id"),
      audit_meta: audit_log_meta(conn)
    )
  end
end
