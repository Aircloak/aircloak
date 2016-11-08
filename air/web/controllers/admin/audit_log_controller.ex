defmodule Air.Admin.AuditLogController do
  @moduledoc false
  use Air.Web, :admin_controller

  alias Air.Utils


  # -------------------------------------------------------------------
  # Air.VerifyPermissions callback
  # -------------------------------------------------------------------

  def permissions do
    %{
      admin: [:index]
    }
  end


  # -------------------------------------------------------------------
  # Actions
  # -------------------------------------------------------------------

  def index(conn, params) do
    from = date_or_default("#{params["from"]}T00:00:00Z", Utils.DateTime.datetime_days_ago(7))
    to = date_or_default("#{params["to"]}T23:59:59Z", Utils.DateTime.datetime_days_in_the_future(1))
    entries = Air.Service.AuditLog.for(%{
      from: from,
      to: to,
      page: params["page"] || 1,
    })
    render(conn, "index.html", audit_logs: entries)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # The datetime is expected to have the format YYYY-MM-DDTHH:MM:SSZ, if it doesn't,
  # or rather, if it cannot be parsed correctly, then the default will be used.
  defp date_or_default(date_string, default) do
    case Ecto.DateTime.cast("#{date_string}") do
      {:ok, date} -> date
      :error -> default
    end
  end
end
