defmodule AircloakCI.Build.Server do
  @moduledoc """
  Behaviour for powering the life cycle of a single CI build of a pull request or a branch.

  Build server is a GenServer-like process. The behaviour will initialize the process state, and then invoke various
  functions from the callback module. The callbacks are mostly similar to GenServer ones, with some build-specific
  additions.

  The callback module can start child jobs using the `start_job/3` function. The behaviour will monitor the lifecycle
  of these jobs, and notify the callback module when they terminate. The behaviour also takes care of the proper
  cleanup of all child jobs.

  The behaviour immediately starts the project preparation job (powered by `AircloakCI.Build.Job.Prepare`).

  The behaviour will also subscribe to notifications from `AircloakCI.RepoDataProvider`, and handle changes. If the
  related PR is no longer pending, the behaviour will terminate the process. If some new commits are pushed to the PR,
  the behaviour will stop all child jobs, and notify the callback module. Finally, the callback module will be notified
  if something else changes in the PR (e.g. status message).
  """
  use GenServer
  require Logger
  alias AircloakCI.Build.Job
  alias AircloakCI.{Github, LocalProject}

  @type state :: %{
          callback_mod: module,
          source_type: source_type,
          source_id: source_id,
          source: source,
          base_branch: Github.API.branch() | nil,
          project: LocalProject.t(),
          data: any,
          jobs: jobs,
          prepared?: boolean
        }

  @type source_type :: :pull_request | :branch | :local

  @type source_id :: any

  @type source :: Github.API.pull_request() | Github.API.branch() | AircloakCI.Build.Local.source()

  @opaque jobs :: %{job_name => pid}

  @type job_name :: String.t()

  @type async_message_result ::
          {:noreply, state}
          | {:noreply, state, timeout | :hibernate}
          | {:stop, reason :: any, state}

  # -------------------------------------------------------------------
  # Behaviour
  # -------------------------------------------------------------------

  @doc "Invoked to find the build source in the provided repo data."
  @callback build_source(source_id, Github.API.repo_data()) ::
              %{source: source, base_branch: Github.Api.branch(), project: LocalProject.t()} | nil

  @doc "Invoked when the process is initializing."
  @callback init(any, state) ::
              {:ok, state}
              | {:ok, state, timeout | :hibernate}
              | :ignore
              | {:stop, reason :: any}

  @doc """
  Invoked when there's a change in the PR.

  This callback is not invoked if some additional commits are pushed. In this case, the build is restarted.
  """
  @callback handle_source_change(state) :: async_message_result

  @doc "Invoked if a child job terminated with the reason `:normal`."
  @callback handle_job_succeeded(job_name, state) :: async_message_result

  @doc "Invoked if a child job terminated with a non-normal reason."
  @callback handle_job_failed(job_name, reason :: any, state) :: async_message_result

  @doc "Invoked to handle a synchronous request issued by `call/3`."
  @callback handle_call(request :: any, {pid, tag :: term}, state) ::
              {:reply, reply, state}
              | {:reply, reply, state, timeout | :hibernate}
              | {:noreply, state}
              | {:noreply, state, timeout | :hibernate}
              | {:stop, reason, reply, state}
              | {:stop, reason, state}
            when reply: term, reason: term

  @doc "Invoked to handle a plain message sent to this process."
  @callback handle_info(message :: any, state) :: async_message_result

  @doc "Invoked to determine whether nightly build can be invoked."
  @callback run_nightly?(state) :: boolean

  # -------------------------------------------------------------------
  # API Functions
  # -------------------------------------------------------------------

  @doc "Starts a build server related to the given pull request."
  @spec start_link(
          module,
          source_type,
          source_id,
          Github.API.repo_data(),
          any,
          GenServer.options()
        ) :: GenServer.on_start()
  def start_link(callback_mod, source_type, source_id, repo_data, arg, gen_server_opts \\ []),
    do:
      GenServer.start_link(
        __MODULE__,
        {callback_mod, source_type, source_id, repo_data, arg},
        gen_server_opts
      )

  @doc "Makes a synchronous request to the given build server."
  @spec call(GenServer.server(), any, pos_integer | :infinity) :: any
  def call(server, request, timeout \\ :timer.seconds(5)), do: GenServer.call(server, request, timeout)

  @doc "Starts the provided function as a child job of the build server."
  @spec start_job(state, job_name, (() -> any)) :: state
  def start_job(state, name, job_fun) do
    :error = Map.fetch(state.jobs, name)

    LocalProject.clear_job_outcome(state.project, name)
    server = self()
    {:ok, new_job} = Task.start_link(fn -> run_job(server, name, job_fun) end)
    Logger.info("job #{name} for `#{LocalProject.name(state.project)}` started")

    put_in(state.jobs[name], new_job)
  end

  @doc "Terminates all currently running child jobs, and restarts the build from scratch."
  @spec restart(state) :: state
  def restart(state), do: state |> terminate_all_jobs() |> start_preparation_job()

  @doc "Returns true if the given job is running."
  @spec running?(state, job_name) :: boolean
  def running?(state, job_name), do: state |> running_jobs() |> Enum.member?(job_name)

  @doc "Returns all currently running jobs."
  @spec running_jobs(state) :: [job_name]
  def running_jobs(state), do: Map.keys(state.jobs)

  @doc "Reports the job result."
  @spec report_result(pid, job_name, :ok | :error | :failure, any) :: :ok
  def report_result(pid, job_name, result, extra_info \\ nil),
    do: GenServer.cast(pid, {:report_result, job_name, result, extra_info})

  @doc "Forces a build of the given job."
  @spec force_build(pid, job_name) :: :ok | {:error, String.t()}
  def force_build(pid, job_name), do: GenServer.call(pid, {:force_build, job_name})

  @doc "Returns the job type, which is the name of the job without the component prefix."
  @spec job_type(job_name) :: String.t()
  def job_type(job_name) do
    case Regex.named_captures(~r/.*_(?<job_type>[^_]+)$/, job_name) do
      %{"job_type" => "compile"} -> "compile"
      %{"job_type" => "test"} -> "test"
      _ -> job_name
    end
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({callback_mod, source_type, source_id, repo_data, arg}) do
    Process.flag(:trap_exit, true)
    AircloakCI.RepoDataProvider.subscribe()
    enqueue_nightly()

    build_source = build_source(callback_mod, source_id, repo_data)
    false = is_nil(build_source)

    %{
      callback_mod: callback_mod,
      source_type: source_type,
      source_id: source_id,
      source: build_source.source,
      base_branch: build_source.base_branch,
      project: build_source.project,
      data: nil,
      jobs: %{},
      prepared?: false
    }
    |> start_preparation_job()
    |> invoke_callback(:init, [arg])
  end

  @impl GenServer
  def handle_call({:force_build, job_name}, _from, state) do
    cond do
      job_name == "all" ->
        state_after_job_termination = terminate_all_jobs(state)
        LocalProject.clear_job_outcomes(state_after_job_termination.project)
        {:reply, :ok, restart(state_after_job_termination)}

      job_type(job_name) in ["prepare", "compile", "test", "compliance"] ->
        LocalProject.mark_forced(state.project, job_name)
        {:reply, :ok, start_forced_jobs(state)}

      true ->
        {:reply, {:error, "don't know how to start job `#{job_name}`"}, state}
    end
  end

  def handle_call(request, from, state), do: invoke_callback(state, :handle_call, [request, from])

  @impl GenServer
  def handle_cast({:report_result, job_name, result, extra_info}, state) do
    AircloakCI.Build.Reporter.report_result(%AircloakCI.Build.Reporter{
      project: state.project,
      source_type: state.source_type,
      source: state.source,
      job_name: job_name,
      log_name: job_name,
      result: result,
      extra_info: extra_info,
      build_log_command: "ci/production.sh build_log #{production_script_target(state)} #{job_name}",
      restart_command: "ci/production.sh force_build #{production_script_target(state)} #{job_name}",
      remote_console_command: remote_console_command(state, job_name)
    })

    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:repo_data, repo_data}, state) do
    case build_source(state.callback_mod, state.source_id, repo_data) do
      nil ->
        Logger.info("shutting down build server `#{__MODULE__}` for `#{LocalProject.name(state.project)}`")

        state = terminate_all_jobs(state)
        LocalProject.remove(state.project)
        {:stop, :shutdown, state}

      build_source ->
        update_source(state, build_source)
    end
  end

  def handle_info({:job_outcome, name, outcome}, state) do
    LocalProject.set_job_outcome(state.project, name, outcome)

    await_shutdown_or_kill(Map.fetch!(state.jobs, name))

    state = state.jobs |> update_in(&Map.delete(&1, name))
    Logger.info("job #{name} for `#{LocalProject.name(state.project)}` finished")

    state = if name == "prepare" and outcome == :ok, do: %{state | prepared?: true}, else: state

    case outcome do
      :ok -> state |> start_forced_jobs() |> handle_job_succeeded(name)
      error -> handle_job_failed(state, name, error)
    end
  end

  def handle_info({:EXIT, pid, reason} = exit_message, state) do
    case Enum.find(state.jobs, &match?({_name, ^pid}, &1)) do
      {name, ^pid} ->
        new_state = update_in(state.jobs, &Map.delete(&1, name))

        case reason do
          :normal ->
            {:noreply, state}

          _other ->
            Logger.error("job #{name} for `#{LocalProject.name(state.project)}` crashed")
            LocalProject.set_job_outcome(state.project, name, :failure)
            handle_job_failed(new_state, name, reason)
        end

      nil ->
        invoke_callback(state, :handle_info, [exit_message])
    end
  end

  def handle_info(:start_nightly_job, state) do
    if invoke_callback(state, :run_nightly?, []) and Enum.empty?(running_jobs(state)),
      do: AircloakCI.Build.Nightly.maybe_start_job(state.project, state.source)

    enqueue_nightly()
    {:noreply, state}
  end

  def handle_info(other, state), do: invoke_callback(state, :handle_info, [other])

  @impl GenServer
  def terminate(reason, state) do
    Logger.info([
      "build server ",
      inspect(state.callback_mod),
      " for ",
      LocalProject.name(state.project),
      " terminating: ",
      Exception.format_exit(reason)
    ])

    terminate_all_jobs(state)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_job(server, name, job_fun) do
    outcome = job_fun.()
    send(server, {:job_outcome, name, outcome})
  end

  defp start_forced_jobs(state) do
    cond do
      LocalProject.forced?(state.project, "prepare") and not running?(state, "prepare") ->
        restart(state)

      state.prepared? ->
        state.project
        |> LocalProject.forced_jobs()
        |> Enum.reduce(state, &start_forced_job(&2, &1))

      true ->
        state
    end
  end

  defp start_forced_job(state, job_name) do
    cond do
      job_name == "prepare" ->
        state

      job_name == "compliance" ->
        Job.Compliance.start_if_possible(state)

      String.ends_with?(job_name, "_test") ->
        Job.Test.start_if_possible(state, String.replace(job_name, ~r/_test$/, ""))

      String.ends_with?(job_name, "_compile") ->
        Job.Compile.start_if_possible(state, String.replace(job_name, ~r/_compile$/, ""))

      true ->
        Logger.warn("unknown forced job `#{job_name}`")
        state
    end
  end

  defp build_source(callback_mod, source_id, repo_data), do: callback_mod.build_source(source_id, repo_data)

  defp terminate_all_jobs(state) do
    pids = Map.values(state.jobs)
    Enum.each(pids, &Process.exit(&1, :shutdown))
    Enum.each(pids, &await_shutdown_or_kill/1)
    %{state | jobs: %{}}
  end

  defp start_preparation_job(state, opts \\ []), do: Job.Prepare.start(%{state | prepared?: false}, opts)

  defp update_source(state, %{source: source, base_branch: base_branch, project: project}) do
    new_state = %{state | source: source, base_branch: base_branch, project: project}

    cond do
      LocalProject.target_sha(new_state.project) != LocalProject.target_sha(state.project) ->
        state_after_job_termination = terminate_all_jobs(new_state)
        LocalProject.clear_job_outcome(state_after_job_termination.project, "prepare")
        {:noreply, start_preparation_job(state_after_job_termination)}

      new_state.source != state.source ->
        invoke_callback(new_state, :handle_source_change, [])

      true ->
        {:noreply, new_state}
    end
  end

  defp await_shutdown_or_kill(pid) do
    receive do
      {:EXIT, ^pid, _reason} -> :ok
    after
      :timer.seconds(5) ->
        Process.exit(pid, :kill)

        receive do
          {:EXIT, ^pid, _reason} -> :ok
        end
    end
  end

  defp handle_job_succeeded(state, "prepare"), do: invoke_callback(state, :handle_job_succeeded, ["prepare"])

  defp handle_job_succeeded(state, job_name), do: invoke_callback(state, :handle_job_succeeded, [job_name])

  defp handle_job_failed(state, "prepare", _reason) do
    LocalProject.clean(state.project)
    {:noreply, start_preparation_job(state, delay: :timer.seconds(10))}
  end

  defp handle_job_failed(state, name, reason), do: invoke_callback(state, :handle_job_failed, [name, reason])

  defp invoke_callback(state, fun, args), do: apply(state.callback_mod, fun, args ++ [state])

  defp enqueue_nightly(), do: Process.send_after(self(), :start_nightly_job, :timer.seconds(1))

  defp remote_console_command(state, job_name) do
    with component_name when not is_nil(component_name) <- component_name(job_name) do
      "ci/production.sh remote_console #{production_script_target(state)} #{component_name}\n"
    end
  end

  defp component_name(job_name) do
    cond do
      String.ends_with?(job_name, "_compile") -> String.replace(job_name, ~r/_compile$/, "")
      String.ends_with?(job_name, "_test") -> String.replace(job_name, ~r/_test$/, "")
      true -> nil
    end
  end

  defp production_script_target(%{source_type: :pull_request, source: pr}), do: "pr #{pr.number}"
  defp production_script_target(%{source_type: :branch, source: branch}), do: "branch #{branch.name}"
  defp production_script_target(%{source_type: :local, source: local}), do: "local #{local.path}"

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts, behaviour_mod: __MODULE__] do
      require Logger
      @behaviour behaviour_mod

      @impl behaviour_mod
      def handle_source_change(state), do: {:noreply, state}

      @impl behaviour_mod
      def handle_job_succeeded(job_name, state) do
        {:noreply, state}
      end

      @impl behaviour_mod
      def handle_job_failed(job_name, crash_reason, state), do: {:noreply, state}

      @impl behaviour_mod
      def handle_call(request, from, state), do: raise("handle_call/3 not implemented in #{inspect(__MODULE__)}")

      @impl behaviour_mod
      def handle_info(msg, state) do
        proc =
          case Process.info(self(), :registered_name) do
            {_, []} -> self()
            {_, name} -> name
          end

        :error_logger.error_msg('~p ~p received unexpected message in handle_info/2: ~p~n', [
          __MODULE__,
          proc,
          msg
        ])

        {:noreply, state}
      end

      defoverridable behaviour_mod
    end
  end
end
