defmodule AircloakCI.Build do
  @moduledoc "Helpers for working with a single build."

  alias AircloakCI.{CmdRunner, Github}
  require Logger

  defstruct [:name, :build_folder, :log_folder, :repo, :base_branch, :update_git_command, :checkout]

  @opaque t :: %__MODULE__{
    name: String.t,
    build_folder: String.t,
    log_folder: String.t,
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
  def for_pull_request(pr) do
    build =
      create_build(%__MODULE__{
        name: "PR #{pr.title} (##{pr.number})",
        build_folder: Path.join(builds_folder(), pr_folder_name(pr)),
        log_folder: Path.join(logs_folder(), pr_folder_name(pr)),
        base_branch: pr.target_branch,
        repo: pr.repo,
        update_git_command: "fetch --force origin pull/#{pr.number}/merge",
        checkout: pr.merge_sha
      })
    set_desired_sha(build, pr.merge_sha)
    build
  end

  @doc "Prepares the build for the given branch."
  @spec for_branch(Github.API.branch) :: t
  def for_branch(branch) do
    build =
      create_build(%__MODULE__{
        name: "branch #{branch.name}",
        build_folder: Path.join(branches_folder(), branch_folder_name(branch.name)),
        log_folder: Path.join(logs_folder(), branch_folder_name(branch.name)),
        base_branch: base_branch(branch.name),
        repo: branch.repo,
        update_git_command: "pull --rebase",
        checkout: branch.name
      })
    set_desired_sha(build, branch.sha)
    build
  end

  @doc "Returns the build folder."
  @spec folder(t) :: String.t
  def folder(build), do:
    build.build_folder

  @doc "Returns the build name."
  @spec name(t) :: String.t
  def name(build), do:
    build.name

  @doc "Initializes the build."
  @spec initialize(t) :: :ok | {:error, String.t}
  def initialize(build) do
    if current_sha(build) == state(build).desired_sha do
      :ok
    else
      Logger.info("initializing build for #{build.name}")
      log(build, "initializing build for #{build.name}")

      with \
        :ok <- clone_repo(build),
        :ok <- cmd(build, "git #{build.update_git_command}"),
        :ok <- cmd(build, "git checkout #{build.checkout}"),
        do: update_state(build, &%{&1 | status: :initialized})
    end
  end

  @doc "Initializes the build from the base build."
  @spec initialize_from(t, t) :: :ok | {:error, String.t}
  def initialize_from(build, base_build) do
    :empty = status(build)
    File.cp_r(git_folder(base_build), git_folder(build))
    cmd(build, "git reset HEAD --hard")
    copy_folder(base_build, build, "tmp")
    copy_folder(base_build, build, Path.join(~w(cloak priv odbc drivers)))
    initialize(build)
  end

  @doc "Compiles the project in the build folder."
  @spec compile(t) :: :ok | {:error, String.t}
  def compile(build), do:
    cmd(build, "ci/run.sh build_cloak", timeout: :timer.minutes(30))

  @doc "Executes the compliance suite in the build folder."
  @spec compliance(t) :: :ok | {:error, String.t}
  def compliance(build), do:
    cmd(build, "ci/run.sh cloak_compliance", timeout: :timer.minutes(10))

  @doc "Ensures that the project in the build folder is compiled."
  @spec ensure_compiled(t) :: :ok | {:error, String.t}
  def ensure_compiled(build) do
    if ci_possible?(build), do: compile(build), else: :ok
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
  @spec remove_old_folders(Github.API.repo_data) :: :ok
  def remove_old_folders(repo_data) do
    remove_except(builds_folder(), Enum.map(repo_data.pull_requests, &pr_folder_name/1))
    remove_except(branches_folder(), Enum.map(repo_data.branches, &branch_folder_name(&1.name)))
  end

  @doc "Returns the CI version for this build."
  @spec ci_version(t) :: nil | non_neg_integer
  def ci_version(build) do
    case File.read(Path.join([src_folder(build), "ci", "VERSION"])) do
      {:ok, contents} -> contents |> String.trim() |> String.to_integer()
      {:error, _reason} -> nil
    end
  end

  @doc "Determines if CI can be invoked in this build."
  @spec ci_possible?(t) :: boolean
  def ci_possible?(build), do:
    initialize(build) == :ok and not is_nil(ci_version(build))

  @doc """
  Returns the build status.

  Possible states are:

    - `:empty` - the build has not yet been initialized
    - `:initialized` - the build source has been retrieved
    - `:started` - the build has been started
    - `:force_start` - the new build has been requested
    - `:finished` - the build has completed
  """
  @spec status(t) :: :empty | :initialized | :started | :force_start | :finished
  def status(build), do:
    state(build).status

  @doc "Sets the build status."
  @spec set_status(t, :started | :finished | :force_start) :: :ok
  def set_status(build, status), do:
    update_state(build, &%{&1 | status: status})

  @doc "Truncates logs for the given build."
  @spec truncate_logs(t) :: :ok
  def truncate_logs(build), do:
    build.log_folder
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.each(&File.write(&1, ""))

  @doc "Returns the SHA of the current head."
  @spec current_sha(t) :: String.t
  def current_sha(build), do:
    # `:os.cmd` is used since `System.cmd` starts a port which causes an :EXIT message to be delivered to the process.
    'cd #{src_folder(build)} && git rev-parse HEAD'
    |> :os.cmd()
    |> to_string()
    |> String.trim()


  # -------------------------------------------------------------------
  # Build folders
  # -------------------------------------------------------------------

  defp create_build(build) do
    File.mkdir_p!(build.build_folder)
    File.mkdir_p!(src_folder(build))
    File.mkdir_p!(build.log_folder)
    build
  end

  defp logs_folder(), do:
    Path.join(AircloakCI.data_folder(), "logs")

  defp cache_folder(), do:
    Path.join(AircloakCI.data_folder(), "cache")

  defp builds_folder(), do:
    Path.join(cache_folder(), "builds")

  defp pr_folder_name(pr), do:
    "pr-#{pr.number}"

  defp branches_folder(), do:
    Path.join(cache_folder(), "branches")
    Application.app_dir(:aircloak_ci, Path.join("priv", "branches"))

  defp branch_folder_name(branch_name), do:
    String.replace(branch_name, "/", "-")

  defp state_file(build), do:
    Path.join(build.build_folder, "state")

  defp src_folder(build), do:
    Path.join(build.build_folder, "src")

  defp git_folder(build), do:
    Path.join(src_folder(build), ".git")

  defp log_path(build), do:
    Path.join(build.log_folder, "build.log")

  defp remove_except(parent_folder, expected_folder_names) do
    existing_folder_names =
      case File.ls(parent_folder) do
        {:ok, folders} -> folders
        _ -> []
      end

    existing_folder_names
    |> Enum.filter(&(not &1 in expected_folder_names))
    |> Enum.each(&(parent_folder |> Path.join(&1) |> File.rm_rf()))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp base_branch("master"), do: nil
  defp base_branch(_not_master), do: "master"

  defp clone_repo(build) do
    if File.exists?(git_folder(build)) do
      :ok
    else
      log(build, "cloning #{build.repo.owner}/#{build.repo.name}")

      CmdRunner.run(
        ~s(git clone git@github.com:#{build.repo.owner}/#{build.repo.name} #{src_folder(build)}),
        timeout: :timer.minutes(1),
        logger: CmdRunner.file_logger(log_path(build))
      )
    end
  end

  defp copy_folder(source_build, target_build, folder) do
    source = Path.join(src_folder(source_build), folder)
    destination = Path.join(src_folder(target_build), folder)
    File.mkdir_p(Path.dirname(destination))
    # Using `cp -a` instead of File.cp_r, since `cp -a` properly handles symlinks
    # `:os.cmd` is used since `System.cmd` starts a port which causes an :EXIT message to be delivered to the process.
    :os.cmd('cp -a #{source} #{destination}')
  end

  defp update_state(build, updater) do
    new_state = build |> state() |> updater.()
    Logger.info("#{build.name} state: #{inspect(new_state)}")
    log(build, "build state: #{inspect(new_state)}")

    build
    |> state_file()
    |> File.write!(:erlang.term_to_binary(new_state))
  end

  defp state(build) do
    try do
      build
      |> state_file()
      |> File.read!()
      |> :erlang.binary_to_term()
    catch _, _ ->
      %{status: :empty, desired_sha: nil}
    end
  end

  defp set_desired_sha(build, desired_sha) do
    if desired_sha != nil && desired_sha != state(build).desired_sha, do:
      update_state(build, &%{&1 | desired_sha: desired_sha})
  end
end
