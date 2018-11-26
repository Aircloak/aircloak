defmodule Cloak.SystemTest.Setup do
  @moduledoc "Setup of system test"

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Sets up the data sources for the system test"
  @spec run() :: :ok
  def run() do
    Compliance.initialize("config", 10, 1)
    Compliance.regenerate_config_from_db("config")
    Cloak.DataSource.reinitialize_all_data_sources()
    :sys.get_state(Cloak.DataSource)
    Supervisor.terminate_child(Cloak.Supervisor, Cloak.AirSocket.Supervisor)
    Supervisor.restart_child(Cloak.Supervisor, Cloak.AirSocket.Supervisor)
    :ok
  end
end
