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

  @doc "Force starts the given nightly job."
  @spec force(LocalProject.t(), String.t(), atom) :: :ok | {:error, String.t()}
  def force(project, component, job), do: GenServer.call(__MODULE__, {:force, project, component, job})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_), do: {:ok, restore_state()}

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

  def handle_call({:force, project, component, job}, _from, state) do
    response =
      with nil <- if(job_running?(), do: {:error, "another nightly job is currently running"}),
           {:ok, job_spec} <- find_job_spec(project, component, job) do
        start_nightly_job(project, job_spec)
        :ok
      end

    {:reply, response, state}
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
    state =
      put_in(
        state.executed_jobs[job_id(project, job_spec)],
        %{date: Date.utc_today(), sha: LocalProject.target_sha(project)}
      )

    persist_state(state)
    state
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
    now = DateTime.utc_now()
    timestamp = :io_lib.format('~b~2..0b~2..0b~2..0b~2..0b', [now.year, now.month, now.day, now.hour, now.minute])
    log_name = Path.join("nightly", Enum.join([job_spec.component, job_spec.job, timestamp], "_"))
    result = AircloakCI.Build.Component.run_job(project, %{job_spec | job: :nightly}, log_name: log_name)
    send(Process.whereis(__MODULE__), {:job_outcome, project, job_spec, result})
  end

  defp persist_state(state) do
    File.mkdir_p(Path.dirname(state_file()))
    File.write(state_file(), :erlang.term_to_binary(Map.take(state, [:executed_jobs])))
  end

  defp restore_state() do
    initial_state = %{executed_jobs: %{}}

    try do
      case File.read(state_file()) do
        {:error, :enoent} ->
          initial_state

        {:ok, content} ->
          saved_state = :erlang.binary_to_term(content)
          Map.merge(initial_state, saved_state)
      end
    catch
      type, error ->
        Logger.error("error reading nightly state: #{Exception.format(type, error, __STACKTRACE__)}")
        initial_state
    end
  end

  defp state_file() do
    file_name = "#{__MODULE__ |> to_string() |> String.replace(~r/^Elixir\./, "")}.state"
    Path.join([AircloakCI.data_folder(), "persist", file_name])
  end

  defp find_job_spec(project, component, job) do
    case project |> LocalProject.nightly_jobs() |> Enum.find(&match?(%{component: ^component, job: ^job}, &1)) do
      nil -> {:error, "can't find the given job"}
      job_spec -> {:ok, job_spec}
    end
  end

  defp cleanup_logs() do
    ~w(#{AircloakCI.LocalProject.logs_folder()} * nightly *)
    |> Path.join()
    |> Path.wildcard()
    |> Stream.filter(&(age_in_days(&1) > 30))
    |> Enum.each(&File.rm/1)
  end

  defp age_in_days(file) do
    file_mtime = NaiveDateTime.from_erl!(File.stat!(file).mtime)
    age_in_sec = NaiveDateTime.diff(NaiveDateTime.utc_now(), file_mtime, :second)
    div(age_in_sec, 60 * 60 * 24)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)

  @doc false
  def child_spec(_) do
    Aircloak.ChildSpec.supervisor(
      [server_spec(), periodic_log_cleanup_spec()],
      strategy: :one_for_one,
      name: __MODULE__.Supervisor
    )
  end

  defp server_spec(), do: %{id: __MODULE__, start: {__MODULE__, :start_link, []}}

  defp periodic_log_cleanup_spec() do
    Periodic.child_spec(
      run: &cleanup_logs/0,
      every: Aircloak.in_env(dev: :timer.minutes(1), else: :timer.hours(1)),
      overlap?: false
    )
  end
end
