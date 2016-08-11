defmodule Air.AuditLogController do
  @moduledoc false
  use Air.Web, :controller

  alias Air.AuditLog


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:index, :load_entries]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, _params) do
    from = date_days_ago(7)
    to = date_days_ago(-1)
    render(conn, "index.html", audit_logs: load_entries_json(from, to))
  end

  def load_entries(conn, params) do
    from = date_or_default("#{params["from"]}T00:00:00Z", date_days_ago(7))
    to = date_or_default("#{params["to"]}T23:59:59Z", date_days_ago(-1))
    json(conn, load_entries_json(from, to))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_entries_json(from, to) do
    (from a in AuditLog,
    where: a.inserted_at >= ^from and a.inserted_at <= ^to,
    order_by: [desc: :inserted_at])
    |> Repo.all()
    |> Enum.map(&AuditLog.for_display/1)
    |> Poison.encode!()
  end

  # The date is expected to have the format YYYY/MM/DD, if it doesn't,
  # then the default will be used
  defp date_or_default(nil, default), do: default
  defp date_or_default(date_string, default) do
    case Ecto.DateTime.cast("#{date_string}") do
      {:ok, date} -> date
      :error -> default
    end
  end

  defp date_days_ago(days) do
    interval = Timex.Time.to_timestamp(days, :days)
    now = Timex.Date.now()
    Timex.subtract(now, interval)
    |> Timex.to_erlang_datetime()
    |> Ecto.DateTime.from_erl()
  end
end
