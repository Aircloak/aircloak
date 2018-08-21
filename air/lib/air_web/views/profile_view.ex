defmodule AirWeb.ProfileView do
  @moduledoc false
  use Air.Web, :view

  defp number_format_settings(conn), do: Air.Service.User.number_format_settings(conn.assigns.current_user)

  defp panel_class(true), do: "panel-success"
  defp panel_class(_), do: "panel-default"

  defp export_name() do
    time = Timex.now() |> Timex.format!("{YYYY}{0M}{0D}{h24}{m}{s}")
    "aircloak_export_#{time}.json"
  end

  defp can_edit?(%{source: :ldap}), do: false
  defp can_edit?(%{source: :native}), do: true
end
