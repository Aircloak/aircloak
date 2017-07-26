defmodule Air.ProfileView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  defp number_format_settings(conn), do:
    Air.Service.User.number_format_settings(conn.assigns.current_user)
end
