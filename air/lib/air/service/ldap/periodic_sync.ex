defmodule Air.Service.LDAP.PeriodicSync do
  @moduledoc "This module is a wrapper around the other LDAP modules that syncs Air with LDAP periodically."

  require Logger
  require Aircloak.DeployConfig

  alias Air.Service.LDAP

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Perform a full LDAP sync by fetching a list of users and groups and applying those to Air."
  @spec run() :: :ok
  def run() do
    Logger.info("Syncing with LDAP.")

    with :ok <- check_config(),
         :ok <- check_license(),
         {:ok, users} <- LDAP.Client.users(),
         {:ok, groups} <- LDAP.Client.groups() do
      groups = LDAP.Normalization.normalize_groups(users, groups)
      LDAP.Sync.sync(users, groups)

      Logger.info("LDAP sync finished.")
    else
      {:error, :ldap_not_configured} ->
        Logger.info("LDAP not configured. Disabling LDAP users and removing LDAP groups if any exist.")
        LDAP.Sync.sync(_users = [], _groups = [])

      {:error, :license_error} ->
        Logger.error(
          "You have configured LDAP sync, but your license does not include support for the LDAP integration. " <>
            "Contact support@aircloak.com if you want to upgrade your license or if you think this is a mistake."
        )

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

  defp check_license() do
    if :ldap in Air.Service.License.features() do
      :ok
    else
      {:error, :license_error}
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
