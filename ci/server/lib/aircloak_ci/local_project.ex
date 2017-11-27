defmodule AircloakCI.LocalProject do
  @moduledoc "Helpers for working with a cloned local project."

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

  @doc "Prepares the local project for the given pull request."
  @spec for_pull_request(Github.API.pull_request) :: t
  def for_pull_request(pr) do
    project =
      create_project(%__MODULE__{
        name: "PR #{pr.title} (##{pr.number})",
        build_folder: Path.join(builds_folder(), pr_folder_name(pr)),
        log_folder: Path.join(logs_folder(), pr_folder_name(pr)),
        base_branch: pr.target_branch,
        repo: pr.repo,
        update_git_command: "fetch --force origin pull/#{pr.number}/merge",
        checkout: pr.merge_sha
      })
    set_desired_sha(project, pr.merge_sha)
    project
  end

  @doc "Prepares the project for the given branch."
  @spec for_branch(Github.API.branch) :: t
  def for_branch(branch) do
    project =
      create_project(%__MODULE__{
        name: "branch #{branch.name}",
        build_folder: Path.join(branches_folder(), branch_folder_name(branch.name)),
        log_folder: Path.join(logs_folder(), branch_folder_name(branch.name)),
        base_branch: base_branch(branch.name),
        repo: branch.repo,
        update_git_command: "pull --rebase",
        checkout: branch.name
      })
    set_desired_sha(project, branch.sha)
    project
  end

  @doc "Returns the build folder."
  @spec folder(t) :: String.t
  def folder(project), do:
    project.build_folder

  @doc "Returns the name of the build which uses this project."
  @spec name(t) :: String.t
  def name(project), do:
    project.name

  @doc "Initializes the project."
  @spec initialize(t) :: :ok | {:error, String.t}
  def initialize(project) do
    if current_sha(project) == state(project).desired_sha do
      :ok
    else
      log_start_stop("initializing local project for #{name(project)}", fn ->
        log(project, "initializing local project for #{name(project)}")

        with \
          :ok <- clone_repo(project),
          :ok <- cmd(project, "git #{project.update_git_command}"),
          :ok <- cmd(project, "git checkout #{project.checkout}"),
          do: update_state(project, &%{&1 | status: :initialized})
      end)
    end
  end

  @doc "Initializes the build from the base build."
  @spec initialize_from(t, t) :: :ok | {:error, String.t}
  def initialize_from(project, base_project) do
    :empty = status(project)
    log_start_stop("copying project for #{name(project)} from #{name(base_project)}", fn ->
      File.cp_r(git_folder(base_project), git_folder(project))
      cmd(project, "git reset HEAD --hard")
      copy_folder(base_project, project, "tmp")
      copy_folder(base_project, project, Path.join(~w(cloak priv odbc drivers)))
    end)
    initialize(project)
  end

  @doc "Compiles the project."
  @spec compile(t) :: :ok | {:error, String.t}
  def compile(project), do:
    log_start_stop("compiling #{name(project)}",
      fn -> cmd(project, "ci/run.sh build_cloak", timeout: :timer.minutes(30)) end)

  @doc "Executes the compliance suite in the project folder."
  @spec compliance(t) :: :ok | {:error, String.t}
  def compliance(project), do:
    log_start_stop("running compliance for #{name(project)}",
      fn -> cmd(project, "ci/run.sh cloak_compliance", timeout: :timer.minutes(10)) end)

  @doc "Ensures that the project in the project folder is compiled."
  @spec ensure_compiled(t) :: :ok | {:error, String.t}
  def ensure_compiled(project) do
    if ci_possible?(project), do: compile(project), else: :ok
  end

  @doc "Executes the given command in the project folder."
  @spec cmd(t, String.t, CmdRunner.opts) :: :ok | {:error, String.t}
  def cmd(project, cmd, opts \\ []), do:
    CmdRunner.run(cmd, [cd: src_folder(project), logger: CmdRunner.file_logger(log_path(project))] ++ opts)

  @doc "Executes the given command in the project folder, raises on error."
  @spec cmd!(t, String.t, CmdRunner.opts) :: :ok
  def cmd!(project, cmd, opts \\ []), do:
    :ok = cmd(project, cmd, opts)

  @doc "Appends the given output to the log."
  @spec log(t, iodata) :: :ok
  def log(project, output), do:
    project
    |> log_path()
    |> CmdRunner.file_logger()
    |> apply([["\naircloak_ci: #{output}\n"]])

  @doc "Returns the contents of the project log."
  @spec log_contents(t) :: binary
  def log_contents(project) do
    case File.read(log_path(project)) do
      {:ok, contents} -> contents
      _ -> ""
    end
  end

  @doc "Removes project folders not needed for any pending pull request."
  @spec remove_old_folders(Github.API.repo_data) :: :ok
  def remove_old_folders(repo_data) do
    remove_except(builds_folder(), Enum.map(repo_data.pull_requests, &pr_folder_name/1))
    remove_except(branches_folder(), Enum.map(repo_data.branches, &branch_folder_name(&1.name)))
  end

  @doc "Returns the CI version for this project."
  @spec ci_version(t) :: nil | non_neg_integer
  def ci_version(project) do
    case File.read(Path.join([src_folder(project), "ci", "VERSION"])) do
      {:ok, contents} -> contents |> String.trim() |> String.to_integer()
      {:error, _reason} -> nil
    end
  end

  @doc "Determines if CI can be invoked in this project."
  @spec ci_possible?(t) :: boolean
  def ci_possible?(project), do:
    initialize(project) == :ok and not is_nil(ci_version(project))

  @doc """
  Returns the build status for this project.

  Possible states are:

    - `:empty` - the build has not yet been initialized
    - `:initialized` - the build source has been retrieved
    - `:started` - the build has been started
    - `:force_start` - the new build has been requested
    - `:finished` - the build has completed
  """
  @spec status(t) :: :empty | :initialized | :started | :force_start | :finished
  def status(project), do:
    state(project).status

  @doc "Sets the build status."
  @spec set_status(t, :started | :finished | :force_start) :: :ok
  def set_status(build, status), do:
    update_state(build, &%{&1 | status: status})

  @doc "Truncates logs for the given project."
  @spec truncate_logs(t) :: :ok
  def truncate_logs(project), do:
    project.log_folder
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.each(&File.write(&1, ""))

  @doc "Returns the SHA of the current head."
  @spec current_sha(t) :: String.t
  def current_sha(project), do:
    # `:os.cmd` is used since `System.cmd` starts a port which causes an :EXIT message to be delivered to the process.
    'cd #{src_folder(project)} && git rev-parse HEAD'
    |> :os.cmd()
    |> to_string()
    |> String.trim()


  # -------------------------------------------------------------------
  # Build folders
  # -------------------------------------------------------------------

  defp create_project(project) do
    File.mkdir_p!(project.build_folder)
    File.mkdir_p!(src_folder(project))
    File.mkdir_p!(project.log_folder)
    project
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

  defp state_file(project), do:
    Path.join(project.build_folder, "state")

  defp src_folder(project), do:
    Path.join(project.build_folder, "src")

  defp git_folder(project), do:
    Path.join(src_folder(project), ".git")

  defp log_path(project), do:
    Path.join(project.log_folder, "build.log")

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

  defp log_start_stop(msg, fun) do
    Logger.info("started #{msg}")
    try do
      fun.()
    after
      Logger.info("finished #{msg}")
    end
  end

  defp base_branch("master"), do: nil
  defp base_branch(_not_master), do: "master"

  defp clone_repo(project) do
    if File.exists?(git_folder(project)) do
      :ok
    else
      log(project, "cloning #{project.repo.owner}/#{project.repo.name}")

      CmdRunner.run(
        ~s(git clone git@github.com:#{project.repo.owner}/#{project.repo.name} #{src_folder(project)}),
        timeout: :timer.minutes(1),
        logger: CmdRunner.file_logger(log_path(project))
      )
    end
  end

  defp copy_folder(source_project, target_project, folder) do
    source = Path.join(src_folder(source_project), folder)
    destination = Path.join(src_folder(target_project), folder)
    File.mkdir_p(Path.dirname(destination))
    # Using `cp -a` instead of File.cp_r, since `cp -a` properly handles symlinks
    # `:os.cmd` is used since `System.cmd` starts a port which causes an :EXIT message to be delivered to the process.
    :os.cmd('cp -a #{source} #{destination}')
  end

  defp update_state(project, updater) do
    new_state = project |> state() |> updater.()
    Logger.info("#{project.name} state: #{inspect(new_state)}")
    log(project, "project state: #{inspect(new_state)}")

    project
    |> state_file()
    |> File.write!(:erlang.term_to_binary(new_state))
  end

  defp state(project) do
    try do
      project
      |> state_file()
      |> File.read!()
      |> :erlang.binary_to_term()
    catch _, _ ->
      %{status: :empty, desired_sha: nil}
    end
  end

  defp set_desired_sha(project, desired_sha) do
    if desired_sha != nil && desired_sha != state(project).desired_sha, do:
      update_state(project, &%{&1 | desired_sha: desired_sha})
  end
end
