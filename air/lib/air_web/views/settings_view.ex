defmodule AirWeb.SettingsView do
  @moduledoc false
  use Air.Web, :view

  defp number_format_settings(conn), do: Air.Service.User.number_format_settings(conn.assigns.current_user)

  defp panel_class(true), do: "card border-success"
  defp panel_class(_), do: "card"

  defp header_class(true), do: "card-header border-success"
  defp header_class(_), do: "card-header"

  defp export_name() do
    time = Timex.now() |> Timex.format!("{YYYY}{0M}{0D}{h24}{m}{s}")
    "aircloak_export_#{time}.json"
  end

  defp can_edit?(%{source: :ldap}), do: false
  defp can_edit?(%{source: :native}), do: true

  defp session_count(conn), do: Air.Service.RevokableToken.count(conn.assigns.current_user, :session)
end
