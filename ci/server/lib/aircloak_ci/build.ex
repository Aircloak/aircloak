defmodule AircloakCI.Build do
  @moduledoc "Helpers for working with a single build."
  alias AircloakCI.CmdRunner


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Initializes the build.

  This function initializes the folder structure for the given build, clones the repo, and merges the target branch into
  the source branch.
  """
  @spec initialize(AircloakCI.Github.pull_request) :: :ok | {:error, String.t}
  def initialize(pr) do
    init_folder(pr)

    clone_repo(pr)
    repo_git!(pr, "reset --hard HEAD")
    repo_git!(pr, "checkout master")
    repo_git!(pr, "pull")
    repo_git!(pr, "checkout #{pr.source_branch}")
    repo_git!(pr, "checkout #{pr.target_branch}")

    case repo_git(pr, "merge --no-ff --no-commit #{pr.source_branch}") do
      :ok -> :ok
      {:error, _} -> {:error, "This branch can't be merged automatically."}
    end
  end


  # -------------------------------------------------------------------
  # Build folders
  # -------------------------------------------------------------------

  defp init_folder(pr) do
    File.mkdir_p!(builds_folder())
    File.mkdir_p!(log_folder(pr))
    File.mkdir_p!(src_folder(pr))
  end

  defp builds_folder(), do:
    Application.app_dir(:aircloak_ci, Path.join(["priv", "builds"]))

  defp build_folder(pr), do:
    Path.join([builds_folder(), encode_branch_folder(pr)])

  defp log_folder(pr), do:
    Path.join([build_folder(pr), "log"])

  defp src_folder(pr), do:
    Path.join([build_folder(pr), "src"])

  defp encode_branch_folder(pr), do:
    {pr.repo.owner, pr.repo.name, pr.source_branch, pr.target_branch}
    |> :erlang.term_to_binary()
    |> Base.encode64(padding: false)


  # -------------------------------------------------------------------
  # Git helpers
  # -------------------------------------------------------------------

  defp clone_repo(pr) do
    unless File.exists?(Path.join(src_folder(pr), ".git")), do:
      CmdRunner.run!(
        ~s(git clone git@github.com:#{pr.repo.owner}/#{pr.repo.name} #{src_folder(pr)}),
        timeout: :timer.minutes(1)
      )
  end

  defp repo_git!(pr, args), do:
    CmdRunner.run!("git #{args}", cd: src_folder(pr))

  defp repo_git(pr, args), do:
    CmdRunner.run("git #{args}", cd: src_folder(pr))
end
