defmodule AircloakCI.Build.Nightly do
  @moduledoc "Execution of nightly jobs."

  use Parent.GenServer
  require Logger
  require Aircloak
  alias AircloakCI.LocalProject

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts one nightly job for the given project."
  @spec maybe_start_job(LocalProject.t()) :: :ok
  def maybe_start_job(project), do: GenServer.call(__MODULE__, {:run_nightly, project})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, %{executed_jobs: %{}}}

  @impl GenServer
  def handle_call({:run_nightly, project}, _from, state) do
    if nightly_enabled?() and Time.utc_now().hour in nightly_hours?() and not job_running?() do
      project
      |> LocalProject.nightly_jobs()
      |> Stream.reject(&executed_today?(state, project, &1))
      |> Stream.filter(&changed?(state, project, &1))
      |> Stream.take(1)
      |> Enum.each(&start_nightly_job(project, &1))
    end

    {:reply, :ok, state}
  end

  @impl GenServer
  def handle_info({:job_outcome, project, job_spec, outcome}, state) do
    report_job_outcome(project, job_spec, outcome)
    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Parent.GenServer callbacks
  # -------------------------------------------------------------------

  @impl Parent.GenServer
  def handle_child_terminated(:nightly_job, {project, job_spec}, _pid, reason, state) do
    if reason != :normal, do: report_job_outcome(project, job_spec, :crash)
    {:noreply, mark_job_as_executed(state, project, job_spec)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp nightly_enabled?(), do: Application.get_env(:aircloak_ci, :run_nightly?, true)

  defp nightly_hours?(), do: Aircloak.in_env(dev: 0..24, test: [], prod: 0..4)

  defp job_running?(), do: Parent.GenServer.child?(:nightly_job)

  defp job_id(project, job_spec), do: {LocalProject.name(project), job_spec.component, job_spec.job}

  defp mark_job_as_executed(state, project, job_spec) do
    put_in(
      state.executed_jobs[job_id(project, job_spec)],
      %{date: Date.utc_today(), sha: LocalProject.target_sha(project)}
    )
  end

  defp executed_today?(state, project, job_spec) do
    today = Date.utc_today()
    match?({:ok, %{date: ^today}}, Map.fetch(state.executed_jobs, job_id(project, job_spec)))
  end

  defp changed?(state, project, job_spec) do
    sha = LocalProject.target_sha(project)
    not match?({:ok, %{sha: ^sha}}, Map.fetch(state.executed_jobs, job_id(project, job_spec)))
  end

  defp report_job_outcome(project, job_spec, outcome),
    do: Logger.info("nightly job #{job_spec.job} for #{LocalProject.name(project)} #{explanation(outcome)}")

  defp explanation(:ok), do: "succeeded"
  defp explanation(:error), do: "failed"
  defp explanation(:crash), do: "crashed"

  defp start_nightly_job(project, job_spec) do
    Parent.GenServer.start_child(%{
      id: :nightly_job,
      start: {Task, :start_link, [fn -> run_nightly_job(project, job_spec) end]},
      meta: {project, job_spec}
    })
  end

  defp run_nightly_job(project, job_spec) do
    Logger.info("starting nightly job #{job_spec.job} for #{LocalProject.name(project)}")
    result = AircloakCI.Build.Component.run_job(project, %{job_spec | job: :nightly})
    send(Process.whereis(__MODULE__), {:job_outcome, project, job_spec, result})
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
