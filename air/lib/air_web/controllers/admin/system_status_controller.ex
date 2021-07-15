defmodule AirWeb.Admin.SystemStatusController do
  @moduledoc """
  Controller for administrators to get a view of the live state of their system.
  """

  use Air.Web, :admin_controller

  alias Plug.CSRFProtection
  alias Air.Service.{AuditLog, Cloak.Stats, Query, User, Warnings, Logs}

  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    render(
      conn,
      "index.html",
      csrf_token: CSRFProtection.get_csrf_token(),
      socket_token: AirWeb.Plug.Session.current_token(conn),
      running_queries: Query.not_finished(),
      cloak_stats: Stats.cloak_stats(),
      login_events: AuditLog.login_events_stats(),
      query_stats: AuditLog.query_stats(),
      token_stats: User.token_stats(),
      problems: Warnings.problems()
    )
  end

  def warnings(conn, _params), do: render(conn, "warnings.html", problems: Air.Service.Warnings.problems())

  def logs_archive(conn, _params) do
    conn =
      conn
      |> put_resp_content_type("text/plain")
      |> put_resp_header("Content-disposition", "attachment; filename=\"logs_archive.txt\"")
      |> send_chunked(200)

    Logs.stream_all(fn stream ->
      stream
      |> Stream.chunk_every(500)
      |> Enum.reduce(conn, fn batch, conn ->
        {:ok, conn} = chunk(conn, Enum.map(batch, &format_log_entry/1))
        conn
      end)
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp format_log_entry(log) do
    timestamp = log.timestamp |> NaiveDateTime.truncate(:millisecond) |> to_string()
    [timestamp, " [#{log.source}@#{log.hostname}] [#{log.level}]: ", log.message, ?\n]
  end
end
