defmodule AircloakCI.Build.Server do
  @moduledoc "Server which handles a build of a single pull request."

  use GenServer, start: {__MODULE__, :start_link, []}, restart: :temporary
  require Logger
  alias AircloakCI.{LocalProject, Github, Queue}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Force starts the build of the given pull request."
  @spec force_build(Github.API.pull_request) :: :ok | {:error, String.t}
  def force_build(pr) do
    fn -> GenServer.call(name(pr), :initialized?) || :timer.sleep(:timer.seconds(1)) end
    |> Stream.repeatedly()
    |> Stream.drop_while(&(&1 != true))
    |> Stream.take(1)
    |> Stream.run()

    GenServer.call(name(pr), :force_build)
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({pr, repo_data}) do
    AircloakCI.RepoDataProvider.subscribe()
    Process.flag(:trap_exit, true)

    project = LocalProject.for_pull_request(pr)
    Logger.info("started build server for #{LocalProject.name(project)}")

    state = %{pr: pr, project: project, init_task: nil, build_task: nil, start: nil}
    {:ok, start_init_task(state, repo_data)}
  end

  @impl GenServer
  def handle_call(:initialized?, _from, state), do:
    {:reply, state.init_task == nil, state}
  def handle_call(:force_build, _from, %{build_task: build_task} = state) when build_task != nil, do:
    {:reply, :ok, state}
  def handle_call(:force_build, _from, state) do
    case check_ci_possibility(state) do
      :ok ->
        LocalProject.set_status(state.project, :force_start)
        {:reply, :ok, maybe_start_build(state)}
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_info({:repo_data, repo_data}, state) do
    case Enum.find(repo_data.pull_requests, &(&1.number == state.pr.number)) do
      nil ->
        Logger.info("shutting down build server for `#{LocalProject.name(state.project)}`")
        {:stop, :shutdown, state}
      pr ->
        {:noreply, update_pr(state, pr, repo_data)}
    end
  end
  def handle_info({:build_result, result}, state) do
    handle_build_finish(state, result, nil)
    {:noreply, state}
  end
  def handle_info({:EXIT, init_task, _reason}, %{init_task: init_task} = state), do:
    {:noreply, maybe_start_build(%{state | init_task: nil})}
  def handle_info({:EXIT, build_task, reason}, %{build_task: build_task} = state) do
    if reason != :normal, do: handle_build_finish(state, :failure, reason)
    {:noreply, %{state | build_task: nil}}
  end
  def handle_info(other, state), do:
    super(other, state)


  # -------------------------------------------------------------------
  # Project initialization
  # -------------------------------------------------------------------

  defp base_projects(pr, repo_data), do:
    # We're deduping, because if the target is master, we end up with two master branches, which causes deadlocks.
    Enum.uniq([
      LocalProject.for_branch(branch!(repo_data, pr.target_branch)),
      LocalProject.for_branch(branch!(repo_data, "master"))
    ])

  defp init_project(pr_project, base_projects), do:
    Queue.exec(project_queue(pr_project), fn -> init_project([pr_project | base_projects]) end)

  defp init_project([project, base_project | rest]) do
    if LocalProject.status(project) == :empty do
      LocalProject.truncate_logs(project)
      # note: no need to queue in `project`, since this is done by the caller
      Queue.exec(project_queue(base_project), fn ->
        :ok = init_project([base_project | rest])
        :ok = LocalProject.initialize_from(project, base_project)
      end)
      :ok = Queue.exec(:compile, fn -> LocalProject.ensure_compiled(project) end)
    else
      :ok
    end
  end
  defp init_project([master_project]), do:
    # note: no need to queue in `master_project`, since this is done by the caller
    :ok = Queue.exec(:compile, fn -> LocalProject.ensure_compiled(master_project) end)

  defp branch!(repo_data, branch_name), do:
    %{} = Enum.find(repo_data.branches, &(&1.name == branch_name))


  # -------------------------------------------------------------------
  # Build lifecycle
  # -------------------------------------------------------------------

  defp update_pr(state, new_pr, repo_data) do
    state = if state.pr.merge_sha != new_pr.merge_sha, do: cancel_outdated_tasks(state, repo_data), else: state
    maybe_start_build(%{state | pr: new_pr, project: LocalProject.for_pull_request(new_pr)})
  end

  defp cancel_outdated_tasks(state, repo_data), do:
    state
    |> cancel_outdated_init(repo_data)
    |> cancel_current_build()

  defp cancel_outdated_init(state, repo_data) do
    if state.init_task != nil do
      Logger.info("cancelling outdated init")
      sync_kill(state.init_task)
      start_init_task(%{state | init_task: nil}, repo_data)
    else
      state
    end
  end

  defp cancel_current_build(state) do
    if state.build_task != nil do
      Logger.info("cancelling outdated build")
      sync_kill(state.build_task)
    else
      state
    end
  end

  defp sync_kill(pid) do
    Process.exit(pid, :kill)
    receive do {:EXIT, ^pid, _reason} -> :ok end
  end

  defp start_init_task(state, repo_data) do
    {:ok, init_task} = Task.start_link(fn -> init_project(state.project, base_projects(state.pr, repo_data)) end)
    %{state | init_task: init_task}
  end

  defp maybe_start_build(%{init_task: nil, build_task: nil} = state) do
    if not build_finished?(state) and check_ci_possibility(state) == :ok do
      case check_start_preconditions(state) do
        :ok ->
          me = self()
          {:ok, build_task} = Task.start_link(fn -> send(me, {:build_result, run_build(state.pr, state.project)}) end)

          %{state | build_task: build_task, start: :erlang.monotonic_time(:second)}

        {:error, status} ->
          send_status_to_github(state.pr, :pending, status)
          state
      end
    else
      state
    end
  end
  defp maybe_start_build(state), do: state

  defp handle_build_finish(state, result, context) do
    diff_sec = :erlang.monotonic_time(:second) - state.start
    time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

    LocalProject.log(state.project, "finished with result `#{result}` in #{time_output} min")

    send_status_to_github(state.pr, github_status(result), description(result))

    Github.post_comment(
      state.pr.repo.owner,
      state.pr.repo.name,
      state.pr.number,
      comment(state, result, context)
    )

    LocalProject.set_status(state.project, :finished)
  end


  # -------------------------------------------------------------------
  # Validation functions
  # -------------------------------------------------------------------

  defp build_finished?(state), do:
    LocalProject.status(state.project) == :finished and LocalProject.current_sha(state.project) == state.pr.merge_sha

  defp check_ci_possibility(state) do
    with \
      {_error, true} <- {"unmergeable", state.pr.mergeable? and state.pr.merge_sha != nil},
      {_error, true} <- {"CI not possible", LocalProject.ci_possible?(state.project)}
    do
      :ok
    else
      {error, false} -> {:error, error}
    end
  end

  defp check_start_preconditions(state) do
    if LocalProject.status(state.project) == :force_start do
      :ok
    else
      with \
        {_status, true} <- {"waiting for Travis builds to succeed", travis_succeeded?(state.pr)},
        {_status, true} <- {"waiting for approval", state.pr.approved?}
      do
        :ok
      else
        {error, false} -> {:error, error}
      end
    end
  end

  defp travis_succeeded?(pr), do:
    (pr.status_checks["continuous-integration/travis-ci/pr"] || %{status: nil}).status == :success and
    (pr.status_checks["continuous-integration/travis-ci/push"] || %{status: nil}).status == :success


  # -------------------------------------------------------------------
  # Build execution
  # -------------------------------------------------------------------

  defp run_build(pr, project) do
    send_status_to_github(pr, :pending, "build started")
    Queue.exec(project_queue(project), fn ->
      with \
        :ok <- LocalProject.truncate_logs(project),
        :ok <- LocalProject.initialize(project),
        :ok <- LocalProject.set_status(project, :started),
        :ok <- run_phase(project, :compile, &LocalProject.compile/1),
        :ok <- run_phase(project, :compliance, &LocalProject.compliance/1)
      do
        :ok
      else
        {:error, reason} ->
          LocalProject.log(project, "error: #{reason}")
          :error
      end
    end)
  end

  defp run_phase(project, queue_id, fun) do
    LocalProject.log(project, "waiting in #{queue_id} queue")
    Queue.exec(queue_id, fn ->
      LocalProject.log(project, "entered #{queue_id} queue")
      fun.(project)
    end)
  end


  # -------------------------------------------------------------------
  # Communication with Github
  # -------------------------------------------------------------------

  defp send_status_to_github(pr, status, description) do
    status_context = "continuous-integration/aircloak/compliance"
    current_description = (pr.status_checks[status_context] || %{description: nil}).description
    if description != current_description, do:
      Github.put_status_check_state(pr.repo.owner, pr.repo.name, pr.sha, status_context, description, status)
  end

  defp github_status(:ok), do: :success
  defp github_status(:error), do: :error
  defp github_status(:failure), do: :failure

  defp description(:ok), do: "build succeeded"
  defp description(:error), do: "build errored"
  defp description(:failure), do: "build failed"

  defp comment(_state, :ok, nil), do:
    "Compliance build succeeded #{happy_emoji()}"
  defp comment(state, :error, nil), do:
    error_comment(state, "Compliance build errored")
  defp comment(state, :failure, crash_reason), do:
    error_comment(state, "Compliance build crashed", "```\n#{Exception.format_exit(crash_reason)}\n```")

  defp error_comment(state, title, extra_info \\ nil), do:
    Enum.join(
      [
        "#{title} #{sad_emoji()}",
        (if not is_nil(extra_info), do: "\n#{extra_info}\n", else: ""),
        "You can see the full build log by running: `ci/production.sh build_log #{state.pr.number}`\n",
        "Log tail:\n", "```", log_tail(state.project), "```"
      ],
      "\n"
    )

  defp happy_emoji(), do: Enum.random(["💯", "👍", "😊", "❤️", "🎉", "👏"])

  defp sad_emoji(), do: Enum.random(["😞", "😢", "😟", "💔", "👿", "🔥"])

  defp log_tail(project) do
    max_lines = 100
    lines = project |> LocalProject.log_contents() |> String.split("\n")

    lines
    |> Enum.drop(max(length(lines) - max_lines, 0))
    |> Enum.join("\n")
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {AircloakCI.Build.Registry, {:pull_request, pr.number}}}

  defp project_queue(project), do: {:project, LocalProject.folder(project)}


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data), do:
    GenServer.start_link(__MODULE__, {pr, repo_data}, name: name(pr))
end
