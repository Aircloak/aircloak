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
    log(pr, "initializing build for `#{pr.title}`")
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
    CmdRunner.run(cmd, [cd: src_folder(pr), logger: CmdRunner.file_logger(log_path(pr))] ++ opts)

  @doc "Executes the given command in the PR build folder, raises on error."
  @spec cmd!(Github.pull_request, String.t, CmdRunner.opts) :: :ok
  def cmd!(pr, cmd, opts \\ []), do:
    :ok = cmd(pr, cmd, opts)

  @doc "Appends the given output to the build log."
  @spec log(Github.pull_request, iodata) :: :ok
  def log(pr, output), do:
    pr
    |> log_path()
    |> CmdRunner.file_logger()
    |> apply([["\naircloak_ci: #{output}\n"]])


  # -------------------------------------------------------------------
  # Build folders
  # -------------------------------------------------------------------

  defp init_folder(pr) do
    File.mkdir_p!(builds_folder())
    File.mkdir_p!(src_folder(pr))
    File.mkdir_p!(log_folder(pr))
    truncate_logs(pr)
  end

  defp src_folder(pr), do:
    Path.join([build_folder(pr), "src"])

  defp log_folder(pr), do:
    Path.join([build_folder(pr), "log"])

  defp truncate_logs(pr), do:
    pr |> log_folder() |> Path.join("*") |> Path.wildcard() |> Enum.each(&File.write(&1, ""))

  defp build_folder(pr), do:
    Path.join([builds_folder(), encode_branch_folder(pr)])

  defp builds_folder(), do:
    Application.app_dir(:aircloak_ci, Path.join(["priv", "builds"]))

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
        timeout: :timer.minutes(1),
        logger: CmdRunner.file_logger(log_path(pr))
      )
  end

  defp log_path(pr), do:
    pr
    |> log_folder()
    |> Path.join("build.log")
end
