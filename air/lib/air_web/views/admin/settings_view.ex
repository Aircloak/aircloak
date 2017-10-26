defmodule Air.Admin.SettingsView do
  @moduledoc false
  use Air.Web, :view

  defp number_format_settings(), do:
    Air.Service.Settings.read()
    |> Map.take([:decimal_digits, :decimal_sep, :thousand_sep])
end
