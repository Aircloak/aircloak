defmodule Air.Service.LDAP do
  @moduledoc "Contains high-level functions for the frontend to interact with LDAP."

  require Aircloak.DeployConfig
  require Logger

  use GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if LDAP has been configured, false otherwise."
  @spec enabled?() :: boolean
  def enabled?(), do: check_config() == :ok

  @doc "Performs an immediate LDAP sync."
  @spec sync() :: :ok | {:error, :license_error | __MODULE__.Client.ldap_error()}
  def sync(), do: GenServer.call(__MODULE__, :sync)

  # -------------------------------------------------------------------
  # GenServer implementation
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, nil}

  @impl GenServer
  def handle_call(:sync, _from, state), do: {:reply, do_sync(), state}

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp do_sync() do
    Logger.info("Syncing with LDAP.")

    with :ok <- check_config(),
         :ok <- check_license(),
         {:ok, users} <- __MODULE__.Client.users(),
         {:ok, groups} <- __MODULE__.Client.groups() do
      groups = __MODULE__.Normalization.normalize_groups(users, groups)
      __MODULE__.Sync.sync(users, groups)

      Logger.info("LDAP sync finished.")

      :ok
    else
      {:error, :ldap_not_configured} ->
        Logger.info("LDAP not configured. Disabling LDAP users and removing LDAP groups if any exist.")
        LDAP.Sync.sync(_users = [], _groups = [])
        :ok

      error ->
        error
    end
  end

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
  def start_link(_), do: GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
