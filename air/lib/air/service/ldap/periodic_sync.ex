defmodule Air.Service.LDAP.PeriodicSync do
  @moduledoc "This module is a wrapper around the other LDAP modules that syncs Air with LDAP periodically."

  require Logger
  require Aircloak.DeployConfig

  alias Air.Service.{LDAP, LDAP.Sync}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Perform a full LDAP sync by fetching a list of users and groups and applying those to Air."
  @spec run() :: :ok
  def run() do
    with :ok <- check_config(),
         {:ok, users} <- LDAP.users(),
         {:ok, groups} <- LDAP.groups() do
      Logger.info("Syncing with LDAP.")
      Sync.sync(users, groups)
      Logger.info("LDAP sync finished.")
    else
      {:error, :ldap_not_configured} ->
        Logger.info("LDAP not configured. Disabling LDAP users and removing LDAP groups if any exist.")
        Sync.sync(_users = [], _groups = [])

      error ->
        Logger.error("LDAP sync failed. Reason: #{inspect(error)}")
    end

    :ok
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp check_config() do
    case Aircloak.DeployConfig.fetch("ldap") do
      {:ok, _} -> :ok
      _ -> {:error, :ldap_not_configured}
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    Aircloak.ChildSpec.supervisor(
      [{Periodic, run: &run/0, every: :timer.hours(1), initial_delay: 0, overlap?: false, id: :ldap_sync}],
      name: __MODULE__,
      strategy: :one_for_one
    )
  end
end
