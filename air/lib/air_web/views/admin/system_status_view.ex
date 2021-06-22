defmodule AirWeb.Admin.SystemStatusView do
  @moduledoc false
  use Air.Web, :view

  def audit_log_url(metadata) do
    time_range =
      URI.encode_query(%{
        from: date_time_to_string(metadata.start_time),
        to: date_time_to_string(DateTime.utc_now())
      })

    events =
      metadata.event_types
      |> Enum.map(&"events[]=#{&1}")
      |> Enum.join("&")

    "/admin/audit_log?#{time_range}&#{events}"
  end

  defp date_time_to_string(dt), do: Timex.format!(dt, "%F %T", :strftime)
end
