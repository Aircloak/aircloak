defmodule Central.KibanaProxyController do
  @moduledoc false
  use Central.Web, :controller

  @options [{:timeout, :timer.seconds(30)}, {:recv_timeout, :timer.minutes(5)}]


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def get(conn, params) do
    HTTPoison.get(url(conn, params), trimmed_headers(conn), @options) |> handle_response(conn)
  end

  def post(conn, params) do
    HTTPoison.post(url(conn, params), body_or_nil(conn), trimmed_headers(conn), @options) |> handle_response(conn)
  end

  def put(conn, params) do
    HTTPoison.put(url(conn, params), body_or_nil(conn), trimmed_headers(conn), @options) |> handle_response(conn)
  end

  def redirect_to_web_interface(conn, _params) do
    redirect(conn, to: "/kibana/app/kibana")
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp body_or_nil(conn), do: conn.private[:raw_body] || ""

  defp handle_response({:ok, result}, conn), do:
    conn
    |> merge_resp_headers(result.headers)
    # Because we are requesting resources cross domains
    |> put_private(:plug_skip_csrf_protection, true)
    |> send_resp(result.status_code, result.body)
  defp handle_response({:error, error}, conn), do:
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(501, "Could not proxy to Kibana: #{error.reason}")

  defp url(conn, params) do
    query_string = conn.query_string
    path = Enum.join(params["path"], "/")
    kibana_url = Central.site_setting("kibana_url")
    "#{kibana_url}/#{path}?#{query_string}"
  end

  defp trimmed_headers(conn), do:
    conn.req_headers
    |> remove("cookie")
    |> remove("accept")
    |> remove("accept-encoding")
    |> remove("host")

  defp remove(headers, what), do:
    Enum.reject(headers, fn({^what, _}) -> true; (_) -> false end)
end
