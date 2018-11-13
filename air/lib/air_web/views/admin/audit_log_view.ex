defmodule AirWeb.Admin.AuditLogView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.User

  defp audit_logs_to_json(audit_logs) do
    audit_logs
    |> Enum.map(fn audit_log ->
      %{
        event: audit_log.event,
        user: User.main_login(audit_log.user),
        time: audit_log.inserted_at,
        metadata: audit_log.metadata
      }
    end)
    |> to_json()
  end
end
