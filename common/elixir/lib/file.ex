defmodule Aircloak.File do
  @moduledoc "Utility for reading configuration files."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Reads a file from the configuration folder"
  @spec read_config_file(atom, String.t) :: Map.t
  def read_config_file(app, path), do:
    config_file(app, path)
    |> File.read!()
    |> Poison.decode!()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp config_file(app, path), do:
    Path.join([Application.app_dir(app, "priv"), "config", path])
end
