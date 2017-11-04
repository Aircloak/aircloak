defmodule AirWeb.ProfileView do
  @moduledoc false;
  use Air.Web, :view

  defp number_format_settings(conn), do:
    Air.Service.User.number_format_settings(conn.assigns.current_user)
end
