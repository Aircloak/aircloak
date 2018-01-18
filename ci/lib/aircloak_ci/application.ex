defmodule AircloakCI.Application do
  @moduledoc false

  use Application


  # -------------------------------------------------------------------
  # Application callbacks
  # -------------------------------------------------------------------

  def start(_type, _args) do
    startup_check()

    AircloakCI.Queue.create_queues()

    Supervisor.start_link(
      [
        AircloakCI.CmdRunner.Supervisor,
        AircloakCI.Github,
        AircloakCI.RepoDataProvider,
        AircloakCI.Container,
        AircloakCI.Build.Service,
        AircloakCI.BuildCleaner,
        AircloakCI.LogCleaner,
      ],
      strategy: :one_for_one,
      name: AircloakCI.Supervisor
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp startup_check() do
    init_data_folder!()
    check_github_access!()
  end

  if Mix.env() != :test do
    defp check_github_access!() do
      with {:error, reason} <- AircloakCI.github_token(), do:
        raise(reason)
    end
  else
    defp check_github_access!(), do: :ok
  end

  defp init_data_folder!() do
    if File.mkdir_p(AircloakCI.data_folder()) != :ok, do:
      raise(
        "Can't create the `#{AircloakCI.data_folder()}` folder! " <>
        "Please create this folder manually and give the ownership to the account running this service."
      )
  end
end
