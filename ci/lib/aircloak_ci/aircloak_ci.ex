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
  @spec force_pr_build(pos_integer) :: :ok | {:error, String.t}
  def force_pr_build(pr_number) do
    repo_data = AircloakCI.Github.repo_data("aircloak", "aircloak")
    case Enum.find(repo_data.pull_requests, &(&1.number == pr_number)) do
      nil -> {:error, "PR ##{pr_number} not found"}
      pr -> AircloakCI.Build.PullRequest.force_build(pr, repo_data)
    end
  end

  @doc "Force starts the build of the given branch."
  @spec force_branch_build(String.t) :: :ok | {:error, String.t}
  def force_branch_build(branch_name) do
    repo_data = AircloakCI.Github.repo_data("aircloak", "aircloak")
    case Enum.find(repo_data.branches, &(&1.name == branch_name)) do
      nil -> {:error, "branch `#{branch_name}` not found"}
      branch -> AircloakCI.Build.Branch.force_build(branch, repo_data)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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
    Path.join(System.user_home(), ".aircloak_ci")
end
