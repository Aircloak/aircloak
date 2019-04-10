defmodule Air.Service.LDAP do
  @moduledoc "Contains high-level functions for the frontend to interact with LDAP."

  require Aircloak.DeployConfig

  @doc "Returns true if LDAP has been configured, false otherwise."
  @spec enabled?() :: boolean
  def enabled?(), do: Aircloak.DeployConfig.fetch("ldap") != :error
end
