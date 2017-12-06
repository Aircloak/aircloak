defmodule AircloakCI.LocalProject do
  @moduledoc "Helpers for working with a cloned local project."

  alias AircloakCI.{CmdRunner, Github}
  require Logger

  defstruct [:name, :build_folder, :log_folder, :repo, :base_branch, :update_git_command, :checkout, :desired_sha]

  @opaque t :: %__MODULE__{
    name: String.t,
    build_folder: String.t,
    log_folder: String.t,
    repo: Github.API.repo,
    base_branch: String.t | nil,
    update_git_command: String.t,
    checkout: String.t,
    desired_sha: String.t
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the local project for the given pull request."
  @spec for_pull_request(Github.API.pull_request) :: t
  def for_pull_request(pr), do:
    create_project(%__MODULE__{
      name: "PR #{pr.title} (##{pr.number})",
      build_folder: Path.join(builds_folder(), pr_folder_name(pr)),
      log_folder: Path.join(logs_folder(), pr_folder_name(pr)),
      base_branch: pr.target_branch,
      repo: pr.repo,
      update_git_command: "fetch --force origin pull/#{pr.number}/merge",
      checkout: pr.merge_sha,
      desired_sha: pr.merge_sha
    })

  @doc "Prepares the project for the given branch."
  @spec for_branch(Github.API.branch) :: t
  def for_branch(branch), do:
    create_project(%__MODULE__{
      name: "branch #{branch.name}",
      build_folder: Path.join(branches_folder(), branch_folder_name(branch.name)),
      log_folder: Path.join(logs_folder(), branch_folder_name(branch.name)),
      base_branch: base_branch(branch.name),
      repo: branch.repo,
      update_git_command: "fetch --force origin #{branch.name}",
      checkout: branch.sha,
      desired_sha: branch.sha
    })

  @doc "Cleans the entire build folder of the project."
  @spec clean(t) :: :ok
  def clean(project) do
    File.rm_rf(project.build_folder)
    File.mkdir_p!(project.build_folder)
  end

  @doc "Returns the name of the build which uses this project."
  @spec name(t) :: String.t
  def name(project), do:
    project.name

  @doc "Brings the local project to the desired sha."
  @spec update_code(t) :: :ok | {:error, String.t}
  def update_code(project) do
    if up_to_date?(project) do
      :ok
    else
      log_start_stop(project, "updating local project git repository for #{name(project)}", fn ->
        with \
          :ok <- clone_repo(project),
          :ok <- cmd(project, "main", "git #{project.update_git_command}"),
          :ok <- cmd(project, "main", "git checkout #{project.checkout}"),
          do: update_state(project, &%{&1 | initialized?: true})
      end)
    end
  end

  @doc "Initializes the build from the base build."
  @spec initialize_from(t, t) :: :ok
  def initialize_from(project, base_project) do
    false = state(project).initialized?
    log_start_stop(project, "copying project for #{name(project)} from #{name(base_project)}", fn ->
      File.cp_r(git_folder(base_project), git_folder(project))
      cmd(project, "main", "git reset HEAD --hard")
      copy_folder(base_project, project, "tmp")
      copy_folder(base_project, project, Path.join(~w(cloak priv odbc drivers)))
      copy_folder(base_project, project, Path.join(~w(ci deps)))
      copy_folder(base_project, project, Path.join(~w(ci _build)))
    end)
    :ok
  end

  @doc "Appends the given output to the log."
  @spec log(t, String.t, iodata) :: :ok
  def log(project, log_name, output), do:
    project
    |> log_path(log_name)
    |> CmdRunner.file_logger()
    |> apply([["aircloak_ci: #{output}\n"]])

  @doc "Executes the provided function, logging the start and finish events."
  @spec log_start_stop(t, String.t, (() -> result)) :: result when result: var
  def log_start_stop(project, msg, fun) do
    Logger.info("started #{msg}")
    log(project, "main", "started #{msg}")
    try do
      fun.()
    after
      Logger.info("finished #{msg}")
      log(project, "main", "finished #{msg}")
    end
  end

  @doc "Truncates the given log of the project."
  @spec truncate_log(t, String.t) :: :ok
  def truncate_log(project, log_name), do:
    project
    |> log_path(log_name)
    |> File.write("")

  @doc "Returns the contents of the project log."
  @spec log_contents(t, String.t) :: binary
  def log_contents(project, log_name) do
    case File.read(log_path(project, log_name)) do
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

  @doc "Determines if CI can be invoked in this project."
  @spec ci_possible?(t) :: boolean
  def ci_possible?(project), do:
    update_code(project) == :ok and not is_nil(ci_version(project))

  @doc "Returns true if the project source has been initialized."
  @spec initialized?(t) :: boolean
  def initialized?(project), do:
    state(project).initialized?

  @doc "Marks the project as finished, and clears the force flag."
  @spec mark_finished(t) :: :ok
  def mark_finished(project), do:
    update_state(project, &%{&1 | forced_at: nil, finished_at: current_sha(project)})

  @doc "Returns true if the build for this project has finished."
  @spec finished?(t) :: boolean
  def finished?(project), do:
    state(project).finished_at == project.desired_sha

  @doc "Marks the project for the force build."
  @spec mark_forced(t) :: :ok
  def mark_forced(project), do:
    update_state(project, &%{&1 | forced_at: project.desired_sha})

  @doc "Returns whether the project has been marked for the force build."
  @spec forced?(t) :: boolean
  def forced?(project), do:
    state(project).forced_at == project.desired_sha

  @doc "Marks the project component as compiled."
  @spec mark_compiled(t, String.t) :: :ok
  def mark_compiled(project, component), do:
    update_state(project, &put_in(&1.compiled_components[component], current_sha(project)))

  @doc "Returns true if the project component is compiled."
  @spec compiled?(t, String.t) :: boolean
  def compiled?(project, component), do:
    up_to_date?(project) and state(project).compiled_components[component] == project.desired_sha

  @doc "Executes the command in the project folder."
  @spec cmd(t, String.t, String.t, CmdRunner.opts) :: :ok | {:error, String.t}
  def cmd(project, log_name, cmd, opts \\ []), do:
    CmdRunner.run(
      cmd,
      Keyword.merge(
        [cd: src_folder(project), logger: CmdRunner.file_logger(log_path(project, log_name))],
        opts
      )
    )

  @doc "Executes a sequence of commands in the project component."
  @spec component_cmds(t, String.t, String.t, [{String.t, CmdRunner.opts} | String.t]) :: :ok | {:error, String.t}
  def component_cmds(project, component, log_name, cmds) do
    case \
      cmds
      |> Stream.map(&component_cmd(project, component, log_name, &1))
      |> Stream.drop_while(&(&1 == :ok))
      |> Enum.take(1)
    do
      [] -> :ok
      [{:error, _reason} = error] -> error
    end
  end


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

  defp log_path(project, log_name), do:
    Path.join(project.log_folder, "#{log_name}.log")

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

  defp ci_version(project) do
    case File.read(Path.join([src_folder(project), "ci", "VERSION"])) do
      {:ok, contents} -> contents |> String.trim() |> String.to_integer()
      {:error, _reason} -> nil
    end
  end

  defp base_branch("master"), do: nil
  defp base_branch(_not_master), do: "master"

  defp clone_repo(project) do
    if File.exists?(git_folder(project)) do
      :ok
    else
      log(project, "main", "cloning #{project.repo.owner}/#{project.repo.name}")

      CmdRunner.run(
        ~s(git clone git@github.com:#{project.repo.owner}/#{project.repo.name} #{src_folder(project)}),
        timeout: :timer.minutes(1),
        logger: CmdRunner.file_logger(log_path(project, "main"))
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
    original_state = state(project)
    new_state = original_state |> updater.() |> Map.take(Map.keys(default_state()))

    if new_state != original_state do
      Logger.info("#{project.name} state: #{inspect(new_state)}")
      log(project, "main", "project state: #{inspect(new_state)}")

      project
      |> state_file()
      |> File.write!(:erlang.term_to_binary(new_state))
    end

    :ok
  end

  defp state(project), do:
    Map.merge(default_state(), deserialize_state(project))

  defp deserialize_state(project) do
    try do
      project
      |> state_file()
      |> File.read!()
      |> :erlang.binary_to_term()
    catch _, _ ->
      %{}
    end
  end

  defp default_state(), do:
    %{initialized?: false, compiled_components: %{}, forced_at: nil, finished_at: nil}

  defp up_to_date?(project), do:
    current_sha(project) == project.desired_sha

  defp current_sha(project), do:
    # `:os.cmd` is used since `System.cmd` starts a port which causes an :EXIT message to be delivered to the process.
    'cd #{src_folder(project)} && git rev-parse HEAD'
    |> :os.cmd()
    |> to_string()
    |> String.trim()

  defp component_cmd(project, component, log_name, {cmd, opts}), do:
    component_cmd(project, component, log_name, cmd, opts)
  defp component_cmd(project, component, log_name, cmd) when is_binary(cmd), do:
    component_cmd(project, component, log_name, cmd, [])

  defp component_cmd(project, component, log_name, cmd, opts), do:
    cmd(project, log_name, cmd, [cd: Path.join(src_folder(project), to_string(component))] ++ opts)
end
