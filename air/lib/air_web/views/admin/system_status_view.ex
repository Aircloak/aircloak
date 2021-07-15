defmodule AirWeb.Admin.SystemStatusView do
  @moduledoc false
  use Air.Web, :view
  import Phoenix.HTML.Link, only: [link: 2]

  alias Air.Schemas.DataSource

  def audit_log_url(metadata) do
    time_range =
      URI.encode_query(%{
        from: date_time_to_string(metadata.start_time),
        to: date_time_to_string(DateTime.utc_now())
      })

    potential_user =
      if metadata[:user] do
        "&users[]=#{metadata[:user][:id]}"
      else
        ""
      end

    events =
      metadata.events
      |> Enum.map(&"events[]=#{&1}")
      |> Enum.join("&")

    "/admin/audit_log?#{time_range}&#{events}#{potential_user}"
  end

  defp date_time_to_string(dt), do: Timex.format!(dt, "%F %T", :strftime)

  defp type(:privacy_policy), do: ""
  defp type(%DataSource{}), do: "Data source"

  defp name(:privacy_policy), do: "Privacy policy"
  defp name(%DataSource{} = resource), do: resource.name

  defp resource_link(conn, :privacy_policy), do: link("More", to: admin_privacy_policy_path(conn, :new))

  defp resource_link(conn, %DataSource{} = resource),
    do: link("More", to: admin_data_source_path(conn, :show, resource.name))

  defp severity(type),
    do:
      type
      |> Atom.to_string()
      |> String.capitalize()
end
