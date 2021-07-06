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
    case LDAP.sync(:timer.minutes(10)) do
      :ok ->
        :ok

      {:error, error} ->
        Logger.error("LDAP: Sync failed. Reason: #{inspect(error)}")
    end

    :ok
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
