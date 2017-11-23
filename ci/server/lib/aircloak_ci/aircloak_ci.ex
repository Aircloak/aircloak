defmodule AircloakCI do
  @moduledoc "Common helper functions."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Folder where the CI service persists various data, such as build caches and logs."
  @spec data_folder() :: String.t
  def data_folder(), do:
    Path.join([System.user_home(), ".aircloak_ci", "data"])

  @doc "Github personal access token used by this service."
  @spec github_token() :: String.t | nil
  def github_token(), do:
    System.get_env("AIRCLOAK_CI_AUTH")
end
