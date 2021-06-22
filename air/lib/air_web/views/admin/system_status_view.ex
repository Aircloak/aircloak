defmodule AirWeb.Admin.SystemStatusView do
  @moduledoc false
  use Air.Web, :view

  def audit_log_url(metadata) do
    start_time = metadata.start_time
    from = "from=#{start_time.year}-#{start_time.month}-#{start_time.day}"
    events =
      metadata.event_types
      |> Enum.map(fn event_type ->
        "events[]=#{event_type}"
      end)
      |> Enum.join("&")
    "/admin/audit_log?#{from}&#{events}"
  end
end
