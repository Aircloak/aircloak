defmodule AircloakCI.Build do
  @moduledoc "Helpers for working with a single build."

  alias AircloakCI.{CmdRunner, Github}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Initializes the build.

  This function initializes the folder structure for the given build, clones the repo, and merges the target branch into
  the source branch.
  """
  @spec initialize(Github.pull_request) :: :ok | {:error, String.t}
  def initialize(pr) do
    init_folder(pr)

    clone_repo(pr)
    cmd!(pr, "git reset --hard HEAD")
    cmd!(pr, "git checkout master")
    cmd!(pr, "git pull")
    cmd!(pr, "git checkout #{pr.source_branch}")
    cmd!(pr, "git checkout #{pr.target_branch}")

    case cmd(pr, "git merge --no-ff --no-commit #{pr.source_branch}") do
      :ok -> :ok
      {:error, _} -> {:error, "this branch can't be merged automatically."}
    end
  end

  @doc "Executes the given command in the PR build folder."
  @spec cmd(Github.pull_request, String.t, CmdRunner.opts) :: :ok | {:error, String.t}
  def cmd(pr, cmd, opts \\ []), do:
    CmdRunner.run(cmd, [cd: src_folder(pr)] ++ opts)

  @doc "Executes the given command in the PR build folder, raises on error."
  @spec cmd!(Github.pull_request, String.t, CmdRunner.opts) :: :ok
  def cmd!(pr, cmd, opts \\ []), do:
    :ok = cmd(pr, cmd, opts)


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
  # Internal functions
  # -------------------------------------------------------------------

  defp clone_repo(pr) do
    unless File.exists?(Path.join(src_folder(pr), ".git")), do:
      CmdRunner.run!(
        ~s(git clone git@github.com:#{pr.repo.owner}/#{pr.repo.name} #{src_folder(pr)}),
        timeout: :timer.minutes(1)
      )
  end
end
