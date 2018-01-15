defmodule Mix.Tasks.AwaitDatabases do
  @moduledoc "Waits for databases to become available."
  @shortdoc "Waits for databases to become available."

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task


  # -------------------------------------------------------------------
  # Mix task interface
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run(_) do
    Air.Repo.configure()
    repo_config = Application.fetch_env!(:air, Air.Repo)
    Aircloak.await_service!(Keyword.fetch!(repo_config, :hostname), Keyword.fetch!(repo_config, :port))

    Central.Repo.configure()
    repo_config = Application.fetch_env!(:central, Central.Repo)
    Aircloak.await_service!(Keyword.fetch!(repo_config, :hostname), Keyword.fetch!(repo_config, :port))
  end
end
