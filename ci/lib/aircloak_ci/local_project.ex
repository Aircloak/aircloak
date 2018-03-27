defmodule AircloakCI.LocalProject do
  @moduledoc "Helpers for working with a cloned local project."

  alias AircloakCI.{CmdRunner, Github}
  require Logger

  defstruct [
    :type,
    :name,
    :build_folder,
    :log_folder,
    :repo,
    :base_branch,
    :update_git_command,
    :checkout,
    :target_sha,
    :source_sha
  ]

  @opaque t :: %__MODULE__{
            type: :branch | :pull_request | :local,
            name: String.t(),
            build_folder: String.t(),
            log_folder: String.t(),
            repo: Github.API.repo(),
            base_branch: String.t() | nil,
            update_git_command: String.t() | nil,
            checkout: String.t() | nil,
            # the commit which we'll checkout
            target_sha: String.t() | nil,
            # the SHA used to store job states (finished, forced)
            source_sha: String.t()
          }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the full path to the desired log file."
  @spec log_file(t, String.t()) :: String.t()
  def log_file(project, log_name), do: Path.join(project.log_folder, "#{log_name}.log")

  @doc "Returns the full path to the log file for the given target and job."
  @spec log_file(String.t(), String.t(), String.t()) :: String.t()
  def log_file("branch", name, job_name),
    do: Path.join([logs_folder(), branch_folder_name(name), "#{job_name}.log"])

  def log_file("pr", number, job_name),
    do:
      Path.join([
        logs_folder(),
        pr_folder_name(%{number: String.to_integer(number)}),
        "#{job_name}.log"
      ])

  @doc "Prepares the local project for the given pull request."
  @spec for_pull_request(Github.API.pull_request()) :: t
  def for_pull_request(pr),
    do:
      create_project(%__MODULE__{
        type: :pull_request,
        name: "PR #{pr.title} (##{pr.number})",
        build_folder: Path.join(builds_folder(), pr_folder_name(pr)),
        log_folder: Path.join(logs_folder(), pr_folder_name(pr)),
        base_branch: pr.target_branch,
        repo: pr.repo,
        update_git_command: "fetch --force origin pull/#{pr.number}/merge",
        checkout: pr.merge_sha,
        # The target SHA is the merge commit.
        target_sha: pr.merge_sha,
        # We'll use the SHA of the source branch HEAD. This ensures that we won't restart finished PR jobs when
        # something has been changed in the target branch.
        source_sha: pr.sha
      })

  @doc "Prepares the project for the given branch."
  @spec for_branch(Github.API.branch()) :: t
  def for_branch(branch),
    do:
      create_project(%__MODULE__{
        type: :branch,
        name: "branch #{branch.name}",
        build_folder: Path.join(branches_folder(), branch_folder_name(branch.name)),
        log_folder: Path.join(logs_folder(), branch_folder_name(branch.name)),
        base_branch: base_branch(branch.name),
        repo: branch.repo,
        update_git_command: "fetch --force origin #{branch.name}",
        checkout: branch.sha,
        target_sha: branch.sha,
        source_sha: branch.sha
      })

  @doc "Prepares the project for a local folder. Useful only in development."
  @spec for_local(String.t()) :: t
  def for_local(path) do
    path = Path.expand(path)

    head_sha =
      String.trim(CmdRunner.run_with_output!("git rev-parse HEAD", timeout: :timer.minutes(1)))

    create_project(%__MODULE__{
      type: :local,
      name: "local #{path}",
      build_folder: path,
      log_folder: Path.join([path, "tmp", "ci", "log"]),
      base_branch: nil,
      repo: %{name: "aircloak", owner: "aircloak"},
      update_git_command: nil,
      checkout: nil,
      target_sha: head_sha,
      source_sha: head_sha
    })
  end

  @doc "Cleans the entire build folder of the project."
  @spec clean(t) :: :ok
  def clean(%{type: :local}), do: :ok

  def clean(project) do
    File.rm_rf(project.build_folder)
    File.mkdir_p!(project.build_folder)
  end

  @doc "Removes the entire build folder of the project."
  @spec remove(t) :: :ok
  def remove(%{type: :local}), do: :ok
  def remove(project), do: File.rm_rf(project.build_folder)

  @doc "Returns the name of the build which uses this project."
  @spec name(t) :: String.t()
  def name(project), do: project.name

  @doc "Returns the target SHA of this project."
  @spec target_sha(t) :: String.t()
  def target_sha(project), do: project.target_sha

  @doc "Brings the local project to the desired sha."
  @spec update_code(t) :: :ok | {:error, String.t()}
  def update_code(project) do
    cond do
      up_to_date?(project) ->
        update_state(project, &%{&1 | initialized?: true})

      is_nil(project.checkout) ->
        :ok

      true ->
        log_start_stop(
          project,
          "updating local project git repository for #{name(project)}",
          fn ->
            with :ok <- do_update_code(project),
                 do: update_state(project, &%{&1 | initialized?: true})
          end
        )
    end
  end

  @doc "Initializes the build from the base build."
  @spec initialize_from(t, t) :: :ok
  def initialize_from(project, base_project) do
    false = state(project).initialized?

    log_start_stop(
      project,
      "copying project for #{name(project)} from #{name(base_project)}",
      fn ->
        File.cp_r(git_folder(base_project), git_folder(project))
        cmd(project, "main", "git reset HEAD --hard")
        copy_folder(base_project, project, "tmp")
        copy_folder(base_project, project, Path.join(~w(cloak priv odbc drivers)))
        copy_folder(base_project, project, Path.join(~w(ci deps)))
        copy_folder(base_project, project, Path.join(~w(ci _build)))
      end
    )

    :ok
  end

  @doc "Appends the given output to the log."
  @spec log(t, String.t(), iodata) :: :ok
  def log(project, log_name, output),
    do:
      project
      |> log_file(log_name)
      |> CmdRunner.file_logger()
      |> apply([["aircloak_ci: #{output}\n"]])

  @doc "Executes the provided function, logging the start and finish events."
  @spec log_start_stop(t, String.t(), (() -> result)) :: result when result: var
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
  @spec truncate_log(t, String.t()) :: :ok
  def truncate_log(project, log_name),
    do:
      project
      |> log_file(log_name)
      |> File.write("")

  @doc "Returns the contents of the project log."
  @spec log_contents(t, String.t()) :: binary
  def log_contents(project, log_name) do
    case File.read(log_file(project, log_name)) do
      {:ok, contents} -> contents
      _ -> ""
    end
  end

  @doc "Removes project folders not needed for any pending pull request."
  @spec remove_old_folders(Github.API.repo_data()) :: :ok
  def remove_old_folders(repo_data) do
    remove_except(builds_folder(), Enum.map(repo_data.pull_requests, &pr_folder_name/1))
    remove_except(branches_folder(), Enum.map(repo_data.branches, &branch_folder_name(&1.name)))
  end

  @doc "Determines if CI can be invoked in this project."
  @spec ci_possible?(t) :: boolean
  def ci_possible?(project),
    do: update_code(project) == :ok and not Enum.empty?(components(project))

  @doc "Returns true if the project source has been initialized."
  @spec initialized?(t) :: boolean
  def initialized?(project), do: state(project).initialized?

  @doc "Returns true if the job for this project has finished."
  @spec finished?(t, String.t()) :: boolean
  def finished?(project, job_name),
    do: project |> job_outcomes() |> Enum.any?(&match?({^job_name, _}, &1))

  @doc "Marks the project for the force build."
  @spec mark_forced(t, String.t()) :: :ok
  def mark_forced(project, job_name),
    do:
      update_state(
        project,
        &%{
          &1
          | forced_jobs: Map.put(&1.forced_jobs, job_name, project.source_sha),
            job_outcomes: Map.delete(&1.job_outcomes, job_name)
        }
      )

  @doc "Returns whether the project has been marked for the force build."
  @spec forced?(t, String.t()) :: boolean
  def forced?(project, job_name),
    do:
      state(project).forced_jobs[job_name] != nil and
        state(project).forced_jobs[job_name] in [project.source_sha, project.target_sha]

  @doc "Returns the list of forced jobs."
  @spec forced_jobs(t) :: [String.t()]
  def forced_jobs(project),
    do:
      state(project).forced_jobs
      |> Enum.filter(fn {_job_name, sha} ->
        sha in [project.source_sha, project.target_sha] and sha != nil
      end)
      |> Enum.map(fn {job_name, _sha} -> job_name end)

  @doc "Returns all known job outcomes."
  @spec job_outcomes(t) :: %{String.t() => any}
  def job_outcomes(project),
    do:
      state(project).job_outcomes
      |> Enum.filter(fn {_job_name, {_, sha}} ->
        sha in [project.source_sha, project.target_sha] and sha != nil
      end)
      |> Enum.map(fn {job_name, {outcome, _sha}} -> {job_name, outcome} end)
      |> Map.new()

  @doc "Returns the outcome of the job, or nil if the job didn't finish."
  @spec job_outcome(t, String.t()) :: nil | :ok | any
  def job_outcome(project, job_name), do: project |> job_outcomes() |> Map.get(job_name)

  @doc "Stores the outcome of the given job and clears the force flag for the job."
  @spec set_job_outcome(t, String.t(), any) :: :ok
  def set_job_outcome(project, job_name, outcome),
    do:
      update_state(
        project,
        &%{
          &1
          | forced_jobs: Map.put(&1.forced_jobs, job_name, nil),
            job_outcomes: Map.put(&1.job_outcomes, job_name, {outcome, project.source_sha})
        }
      )

  @doc "Clears the outcome of the given job."
  @spec clear_job_outcome(t, String.t()) :: :ok
  def clear_job_outcome(project, job_name),
    do: update_state(project, &%{&1 | job_outcomes: Map.delete(&1.job_outcomes, job_name)})

  @doc "Clears all known outcomes."
  @spec clear_job_outcomes(t) :: :ok
  def clear_job_outcomes(project), do: update_state(project, &%{&1 | job_outcomes: %{}})

  @doc "Executes the command in the project folder."
  @spec cmd(t, String.t(), String.t(), CmdRunner.opts()) :: :ok | {:error, String.t()}
  def cmd(project, log_name, cmd, opts \\ []),
    do:
      CmdRunner.run(
        cmd,
        Keyword.merge(
          [cd: src_folder(project), logger: CmdRunner.file_logger(log_file(project, log_name))],
          opts
        )
      )

  @doc "Executes a sequence of commands in the project component."
  @spec component_cmds(t, String.t(), String.t(), [{String.t(), CmdRunner.opts()} | String.t()]) ::
          :ok | {:error, String.t()}
  def component_cmds(project, component, log_name, cmds) do
    case cmds
         |> Stream.map(&component_cmd(project, component, log_name, &1))
         |> Stream.drop_while(&(&1 == :ok))
         |> Enum.take(1) do
      [] -> :ok
      [{:error, _reason} = error] -> error
    end
  end

  @doc "Returns the folder where the source files of the project are contained."
  @spec src_folder(t) :: String.t()
  def src_folder(%{type: :local} = project), do: project.build_folder
  def src_folder(project), do: Path.join(project.build_folder, "src")

  @doc "Retrieves the list of commands for the given job."
  @spec commands(t, String.t(), atom) :: [String.t()] | {:error, :no_ci}
  def commands(project, component, job) do
    case [
           Path.join([src_folder(project) | ~w(#{component} ci jobs.exs)]),
           # supported for legacy reasons
           Path.join([src_folder(project) | ~w(ci scripts #{component}_commands.exs)])
         ]
         |> Stream.filter(&File.exists?/1)
         |> Enum.map(&Code.eval_file/1) do
      [] ->
        {:error, :no_ci}

      [{commands_map, _}] ->
        fallback_key =
          case job do
            # supported for legacy reasons
            :test ->
              :standard_test

            _other ->
              nil
          end

        Map.get(commands_map, job, Map.get(commands_map, fallback_key, []))
    end
  end

  @doc "Returns the list of components for which CI can be executed."
  @spec components(t) :: [String.t()]
  def components(project),
    do:
      project
      |> src_folder()
      |> File.ls!()
      |> Stream.filter(&File.dir?(Path.join(src_folder(project), &1)))
      |> Stream.reject(&String.starts_with?(&1, "."))
      |> filter_components()
      |> Enum.reject(&match?({:error, _}, commands(project, &1, :compile)))

  @doc "Returns the location of logs folder."
  @spec logs_folder() :: String.t()
  def logs_folder(), do: Path.join(AircloakCI.data_folder(), "logs")

  # -------------------------------------------------------------------
  # Build folders
  # -------------------------------------------------------------------

  defp create_project(project) do
    File.mkdir_p!(project.build_folder)
    File.mkdir_p!(src_folder(project))
    File.mkdir_p!(project.log_folder)
    project
  end

  defp cache_folder(), do: Path.join(AircloakCI.data_folder(), "cache")

  defp builds_folder(), do: Path.join(cache_folder(), "builds")

  defp pr_folder_name(pr), do: "pr-#{pr.number}"

  defp branches_folder(), do: Path.join(cache_folder(), "branches")
  Application.app_dir(:aircloak_ci, Path.join("priv", "branches"))

  defp branch_folder_name(branch_name), do: String.replace(branch_name, "/", "-")

  defp state_file(%{type: :local} = project),
    do: Path.join([project.build_folder, "tmp", "ci", "state"])

  defp state_file(project), do: Path.join(project.build_folder, "state")

  defp git_folder(project), do: Path.join(src_folder(project), ".git")

  defp remove_except(parent_folder, expected_folder_names) do
    existing_folder_names =
      case File.ls(parent_folder) do
        {:ok, folders} -> folders
        _ -> []
      end

    existing_folder_names
    |> Enum.filter(&(not (&1 in expected_folder_names)))
    |> Enum.each(&(parent_folder |> Path.join(&1) |> File.rm_rf()))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp base_branch("master"), do: nil
  defp base_branch(_not_master), do: "master"

  defp do_update_code(%{type: :local}), do: :ok

  defp do_update_code(project) do
    with :ok <- clone_repo(project),
         :ok <-
           cmd(project, "main", "git #{project.update_git_command}", timeout: :timer.minutes(5)),
         do: cmd(project, "main", "git checkout #{project.checkout}")
  end

  defp clone_repo(project) do
    if File.exists?(git_folder(project)) do
      :ok
    else
      log(project, "main", "cloning #{project.repo.owner}/#{project.repo.name}")

      CmdRunner.run(
        ~s(git clone git@github.com:#{project.repo.owner}/#{project.repo.name} #{
          src_folder(project)
        }),
        timeout: :timer.minutes(5),
        logger: CmdRunner.file_logger(log_file(project, "main"))
      )
    end
  end

  defp copy_folder(source_project, target_project, folder) do
    source = Path.join(src_folder(source_project), folder)
    destination = Path.join(src_folder(target_project), folder)
    File.mkdir_p(Path.dirname(destination))
    # Using `cp -a` instead of File.cp_r, since `cp -a` properly handles symlinks
    CmdRunner.run("cp -a #{source} #{destination}")
  end

  defp update_state(project, updater) do
    original_state = state(project)
    new_state = original_state |> updater.() |> Map.take(Map.keys(default_state()))

    if new_state != original_state do
      log(project, "main", "project state: #{inspect(new_state)}")

      project
      |> state_file()
      |> File.write!(:erlang.term_to_binary(new_state))
    end

    :ok
  end

  defp state(project), do: Map.merge(default_state(), deserialize_state(project))

  defp deserialize_state(project) do
    [state_file(project), old_state_file(project)]
    |> Enum.find(&File.exists?/1)
    |> File.read!()
    |> :erlang.binary_to_term()
    |> Map.take(Map.keys(default_state()))
  catch
    _, _ ->
      %{}
  end

  defp old_state_file(project),
    # supported for legacy reasons
    do: project |> state_file() |> Path.dirname() |> Path.join("state_2")

  defp default_state(), do: %{initialized?: false, forced_jobs: %{}, job_outcomes: %{}}

  defp up_to_date?(project) do
    case CmdRunner.run_with_output("cd #{src_folder(project)} && git rev-parse HEAD") do
      {:ok, sha} -> String.trim(sha) == project.target_sha
      {:error, _} -> false
    end
  end

  defp component_cmd(project, component, log_name, {cmd, opts}),
    do: component_cmd(project, component, log_name, cmd, opts)

  defp component_cmd(project, component, log_name, cmd) when is_binary(cmd),
    do: component_cmd(project, component, log_name, cmd, [])

  defp component_cmd(project, component, log_name, cmd, opts),
    do:
      cmd(
        project,
        log_name,
        cmd,
        [cd: Path.join(src_folder(project), to_string(component))] ++ opts
      )

  defp filter_components(components) do
    components
    |> Enum.reject(&(&1 in ["tmp"]))
    |> Enum.filter(
      &include_component?(&1, Application.get_env(:aircloak_ci, :components_filter, :all))
    )
  end

  defp include_component?(_component, :all), do: true

  defp include_component?(component, {:except, blacklisted}),
    do: not Enum.member?(blacklisted, component)

  defp include_component?(component, {:only, whitelisted}),
    do: Enum.member?(whitelisted, component)
end
