defmodule AircloakCI.Job.Server do
  @moduledoc "Server which handles a build of a single pull request."

  use GenServer, start: {__MODULE__, :start_link, []}
  require Logger
  alias AircloakCI.{Build, Github}


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

    build = Build.for_pull_request(pr)
    {:ok, init_task} = Task.start_link(fn -> init_build(build, pr, repo_data) end)
    {:ok, %{
      repo_data: repo_data,
      pr: pr,
      build: build,
      init_task: init_task,
      build_task: nil,
      start: nil
    }}
  end

  @impl GenServer
  def handle_call(:initialized?, _from, state), do:
    {:reply, state.init_task == nil, state}
  def handle_call(:force_build, _from, %{build_task: build_task} = state) when build_task != nil, do:
    {:reply, :ok, state}
  def handle_call(:force_build, _from, state) do
    case check_ci_possibility(state) do
      :ok ->
        Build.set_status(state.build, :force_start)
        {:reply, :ok, maybe_start_build(state)}
      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  @impl GenServer
  def handle_info({:repo_data, repo_data}, state) do
    case Enum.find(repo_data.pull_requests, &(&1.number == state.pr.number)) do
      nil ->
        Logger.info("shutting down build server for `#{Build.name(state.build)}`")
        {:stop, :shutdown, state}
      pr ->
        {:noreply, %{state | repo_data: repo_data} |> update_pr(pr) |> maybe_start_build()}
    end
  end
  def handle_info({:job_result, result}, state) do
    handle_build_finish(state, build_status(result), nil)
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
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {AircloakCI.Job.Registry, {:pull_request, pr.number}}}

  defp init_build(pr_build, pr, repo_data) do
    Build.truncate_logs(pr_build)
    init_build([
      pr_build,
      Build.for_branch(branch!(repo_data, pr.target_branch)),
      Build.for_branch(branch!(repo_data, "master"))
    ])
  end

  defp init_build([build, base_build | rest]) do
    if Build.status(build) == :empty do
      Build.log(build, "initializing from #{Build.name(base_build)}")
      :ok = init_build([base_build | rest])
      :ok = Build.initialize_from(build, base_build)
      :ok = Build.ensure_compiled(build)
    else
      :ok
    end
  end
  defp init_build([master_build]), do:
    :ok = Build.ensure_compiled(master_build)

  defp update_pr(state, pr), do:
    %{maybe_cancel_current_build(state, pr) | pr: pr}

  defp maybe_cancel_current_build(%{build_task: build_task} = state, pr) do
    if state.pr.merge_sha != pr.merge_sha and build_task != nil do
      Logger.info("cancelling outdated build")
      Process.exit(build_task, :kill)
      receive do {:EXIT, ^build_task, _reason} -> :ok end
      %{state | build_task: nil}
    else
      state
    end
  end

  defp maybe_start_build(%{init_task: nil, build_task: nil} = state) do
    if not build_finished?(state) and check_ci_possibility(state) == :ok do
      case check_start_preconditions(state) do
        :ok ->
          me = self()
          {:ok, build_task} = Task.start_link(fn -> send(me, {:job_result, run_build(state.build)}) end)

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

  defp build_finished?(state), do:
    Build.status(state.build) == :finished and Build.current_sha(state.build) == state.pr.merge_sha

  defp check_ci_possibility(state) do
    with \
      {_error, true} <- {"unmergeable", state.pr.mergeable? and state.pr.merge_sha != nil},
      {_error, true} <- {"CI not possible", Build.ci_possible?(state.build)}
    do
      :ok
    else
      {error, false} -> {:error, error}
    end
  end

  defp check_start_preconditions(state) do
    if Build.status(state.build) == :force_start do
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

  defp send_status_to_github(pr, status, description) do
    status_context = "continuous-integration/aircloak/compliance"
    current_description = (pr.status_checks[status_context] || %{description: nil}).description
    if description != current_description, do:
      Github.put_status_check_state(pr.repo.owner, pr.repo.name, pr.sha, status_context, description, status)
  end

  defp run_build(build) do
    with \
      :ok <- Build.truncate_logs(build),
      :ok <- Build.set_status(build, :started),
      :ok <- run_phase(build, "cloak build", &Build.compile/1),
      :ok <- run_phase(build, "compliance", &Build.cmd(&1, "ci/run.sh cloak_compliance", timeout: :timer.minutes(10)))
    do
      :ok
    else
      {:error, reason} ->
        Build.log(build, "error: #{reason}")
        :error
    end
  end

  defp run_phase(build, title, fun) do
    Build.log(build, "starting #{title}")
    fun.(build)
  end

  defp build_status(:ok), do: :success
  defp build_status(:error), do: :error

  defp handle_build_finish(state, build_status, context) do
    diff_sec = :erlang.monotonic_time(:second) - state.start
    time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

    Build.log(state.build, "finished with status `#{build_status}` in #{time_output} min")

    send_status_to_github(state.pr, build_status, description(build_status))
    Github.post_comment(
      state.pr.repo.owner,
      state.pr.repo.name,
      state.pr.number,
      comment(build_status, state.build, context)
    )

    Build.set_status(state.build, :finished)
  end

  defp description(:success), do: "build succeeded"
  defp description(:error), do: "build errored"
  defp description(:failure), do: "build failed"

  defp comment(:success, _build, nil), do:
    "Compliance build succeeded 👍"
  defp comment(:error, build, nil), do:
    Enum.join(["Compliance build errored 😞", "", "Log tail:", "```", log_tail(build), "```"], "\n")
  defp comment(:failure, build, crash_reason), do:
    Enum.join(
      [
        "Compliance build crashed 😞", "",
        "```", Exception.format_exit(crash_reason), "```", "",
        "Log tail:", "```", log_tail(build), "```"
      ],
      "\n"
    )

  defp log_tail(build) do
    max_lines = 100
    lines = build |> Build.log_contents() |> String.split("\n")

    lines
    |> Enum.drop(max(length(lines) - max_lines, 0))
    |> Enum.join("\n")
  end

  defp branch!(repo_data, branch_name), do:
    %{} = Enum.find(repo_data.branches, &(&1.name == branch_name))


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data), do:
    GenServer.start_link(__MODULE__, {pr, repo_data}, name: name(pr))
end
