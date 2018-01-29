defmodule AircloakCI do
  @moduledoc "Common helper functions."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Folder where the CI service persists various data, such as build caches and logs."
  @spec data_folder() :: String.t
  def data_folder(), do:
    Path.join(home_folder(), "data")

  @doc "Returns the github personal access token used by this service."
  @spec github_token() :: {:ok, String.t} | {:error, String.t}
  def github_token() do
    with {:ok, config} <- user_config(), do:
      config_value(config, "github_access_token")
  end

  @doc "Returns the github personal access token used by this service, raises on error."
  @spec github_token!() :: String.t
  def github_token!() do
    {:ok, token} = github_token()
    token
  end

  @doc "Force starts the build of the given pull request."
  @spec force_build(String.t, String.t, String.t) :: :ok | {:error, String.t}
  def force_build("local", path, job_name) do
    path
    |> AircloakCI.Build.Local.ensure_started()
    |> AircloakCI.Build.Server.force_build(job_name)
  end
  def force_build(target_type, target_id, job_name) do
    with \
      repo_data = AircloakCI.Github.repo_data("aircloak", "aircloak"),
      {:ok, _pid} <- ensure_started("branch", "master", repo_data),
      {:ok, pid} <- ensure_started(target_type, target_id, repo_data),
      do: AircloakCI.Build.Server.force_build(pid, job_name)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp ensure_started("branch", branch_name, repo_data) do
    case Enum.find(repo_data.branches, &(&1.name == branch_name)) do
      nil -> {:error, "branch `#{branch_name}` not found"}
      branch -> {:ok, AircloakCI.Build.Branch.ensure_started(branch, repo_data)}
    end
  end
  defp ensure_started("pr", pr_number, repo_data) do
    case Enum.find(repo_data.pull_requests, &(&1.number == String.to_integer(pr_number))) do
      nil -> {:error, "PR ##{pr_number} not found"}
      pr -> {:ok, AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)}
    end
  end

  defp user_config() do
    with \
      :error <- Application.fetch_env(:aircloak_ci, :user_config),
      {:ok, config} <- read_user_config()
    do
      Application.put_env(:aircloak_ci, :user_config, config)
      {:ok, config}
    end
  end

  defp read_user_config() do
    with \
      {:ok, config_contents} <- read_config_file(),
      {:error, _} <- Poison.decode(config_contents),
      do: {:error, "config file is not a valid JSON"}
  end

  defp read_config_file() do
    with {:error, _} <- File.read(config_file()), do:
      {:error, "can't read from `#{config_file()}`"}
  end

  defp config_value(config, key) do
    with :error <- Map.fetch(config, key), do:
      {:error, "missing config `#{key}`"}
  end

  defp config_file(), do:
    Path.join(home_folder(), "config.json")

  defp home_folder(), do:
    Application.get_env(:aircloak_ci, :home_folder, Path.join(System.user_home(), ".aircloak_ci"))
end
