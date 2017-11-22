defmodule AircloakCI.Build do
  @moduledoc "Helpers for working with a single build."

  alias AircloakCI.{CmdRunner, Github}
  require Logger

  defstruct [:name, :folder, :repo, :base_branch, :update_git_command, :checkout]

  @opaque t :: %__MODULE__{
    name: String.t,
    folder: String.t,
    repo: Github.API.repo,
    base_branch: String.t | nil,
    update_git_command: String.t,
    checkout: String.t,
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the build for the given pull request."
  @spec for_pull_request(Github.API.pull_request) :: t
  def for_pull_request(pr), do:
    init_folder(%__MODULE__{
      name: "PR #{pr.title} (##{pr.number})",
      folder: Path.join(builds_folder(), to_string(pr.number)),
      base_branch: pr.target_branch,
      repo: pr.repo,
      update_git_command: "fetch --force origin pull/#{pr.number}/merge",
      checkout: pr.merge_sha
    })

  @doc "Initializes the build."
  @spec initialize(t) :: :ok | {:error, String.t}
  def initialize(build) do
    Logger.info("initializing build for #{build.name}")
    log(build, "initializing build for #{build.name}")

    with \
      :ok <- clone_repo(build),
      :ok <- cmd(build, "git #{build.update_git_command}"),
      do: cmd(build, "git checkout #{build.checkout}")
  end

  @doc "Executes the given command in the build folder."
  @spec cmd(t, String.t, CmdRunner.opts) :: :ok | {:error, String.t}
  def cmd(build, cmd, opts \\ []), do:
    CmdRunner.run(cmd, [cd: src_folder(build), logger: CmdRunner.file_logger(log_path(build))] ++ opts)

  @doc "Executes the given command in the build folder, raises on error."
  @spec cmd!(t, String.t, CmdRunner.opts) :: :ok
  def cmd!(build, cmd, opts \\ []), do:
    :ok = cmd(build, cmd, opts)

  @doc "Appends the given output to the build log."
  @spec log(t, iodata) :: :ok
  def log(build, output), do:
    build
    |> log_path()
    |> CmdRunner.file_logger()
    |> apply([["\naircloak_ci: #{output}\n"]])

  @doc "Returns the contents of the build log."
  @spec log_contents(t) :: binary
  def log_contents(build) do
    case File.read(log_path(build)) do
      {:ok, contents} -> contents
      _ -> ""
    end
  end

  @doc "Removes build folders not needed for any pending pull request."
  @spec remove_old_folders([Github.API.pull_request]) :: :ok
  def remove_old_folders(existing_pull_requests) do
    existing_folder_names =
      case File.ls(builds_folder()) do
        {:ok, folders} -> folders
        _ -> []
      end

    expected_folder_names = Enum.map(existing_pull_requests, &to_string(&1.number))

    existing_folder_names
    |> Enum.filter(&(not &1 in expected_folder_names))
    |> Enum.each(&(builds_folder() |> Path.join(&1) |> File.rm_rf()))
  end

  @doc "Returns the CI version for this build."
  @spec ci_version(t) :: nil | non_neg_integer
  def ci_version(build) do
    case File.read(Path.join([src_folder(build), "ci", "VERSION"])) do
      {:ok, contents} -> contents |> String.trim() |> String.to_integer()
      {:error, _reason} -> nil
    end
  end


  # -------------------------------------------------------------------
  # Build folders
  # -------------------------------------------------------------------

  defp init_folder(build) do
    File.mkdir_p!(src_folder(build))
    File.mkdir_p!(log_folder(build))
    truncate_logs(build)
    build
  end

  defp src_folder(build), do:
    Path.join(build.folder, "src")

  defp log_folder(build), do:
    Path.join(build.folder, "log")

  defp git_folder(build), do:
    Path.join(src_folder(build), ".git")

  defp truncate_logs(build), do:
    build.folder |> Path.join("*") |> Path.wildcard() |> Enum.each(&File.write(&1, ""))

  defp builds_folder(), do:
    Application.app_dir(:aircloak_ci, Path.join("priv", "builds"))

  defp branches_folder(), do:
    Application.app_dir(:aircloak_ci, Path.join("priv", "branches"))

  defp log_path(build), do:
    build
    |> log_folder()
    |> Path.join("build.log")


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp base_branch("master"), do: nil
  defp base_branch(_not_master), do: "master"

  defp clone_repo(build) do
    if File.exists?(git_folder(build)) do
      :ok
    else
      do_clone_repo(build)
    end
  end

  defp do_clone_repo(%__MODULE__{base_branch: nil} = build) do
    log(build, "cloning #{build.repo.owner}/#{build.repo.name}")

    CmdRunner.run(
      ~s(git clone git@github.com:#{build.repo.owner}/#{build.repo.name} #{src_folder(build)}),
      timeout: :timer.minutes(1),
      logger: CmdRunner.file_logger(log_path(build))
    )
  end
  defp do_clone_repo(build) do
    log(build, "waiting for #{build.base_branch} to become available")
    base_build = for_branch(build.repo, build.base_branch)
    with :ok <- initialize(base_build) do
      File.cp_r(git_folder(base_build), git_folder(build))
      cmd(build, "git reset HEAD --hard")
    end
  end

  defp for_branch(repo, branch_name), do:
    init_folder(%__MODULE__{
      name: "branch #{branch_name}",
      folder: Path.join(branches_folder(), Base.encode64(branch_name, padding: false)),
      base_branch: base_branch(branch_name),
      repo: repo,
      update_git_command: "pull --rebase",
      checkout: branch_name
    })
end
