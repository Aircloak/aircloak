defmodule Air.Admin.AuditLogController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.{AuditLog, Utils}


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
    from = Utils.DateTime.datetime_days_ago(7)
    to = Utils.DateTime.datetime_days_in_the_future(1)
    render(conn, "index.html", audit_logs: load_entries_json(from, to))
  end

  def load_entries(conn, params) do
    from = date_or_default("#{params["from"]}T00:00:00Z", Utils.DateTime.datetime_days_ago(7))
    to = date_or_default("#{params["to"]}T23:59:59Z", Utils.DateTime.datetime_days_in_the_future(1))
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

  # The datetime is expected to have the format YYYY-MM-DDTHH:MM:SSZ, if it doesn't,
  # or rather, if it cannot be parsed correctly, then the default will be used.
  defp date_or_default(date_string, default) do
    case Ecto.DateTime.cast("#{date_string}") do
      {:ok, date} -> date
      :error -> default
    end
  end
end
