defmodule Air.Service.LDAP.PeriodicSync do
  require Logger
  require Aircloak.DeployConfig

  alias Air.Service.{LDAP, LDAP.Sync}

  def run() do
    with :ok <- check_config(),
         {:ok, users} <- LDAP.users(),
         {:ok, groups} <- LDAP.groups() do
      Sync.sync(users, groups)
    else
      {:error, :ldap_not_configured} -> :ldap_not_configured
      error -> Logger.warn("LDAP sync failed. Reason: #{inspect(error)}")
    end
  end

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
      [{Periodic, run: &run/0, every: :timer.hours(1), overlap?: false, id: :ldap_sync}],
      name: __MODULE__,
      strategy: :one_for_one
    )
  end
end
