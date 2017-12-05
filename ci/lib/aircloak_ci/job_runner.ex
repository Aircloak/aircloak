defmodule AircloakCI.JobRunner do
  @moduledoc """
  Behaviour for running a CI job.

  CI job is a process which performs some stage of the build of the given project. It can power a single task, such as
  execution of compliance tests, or a more complex workflow, such as the orchestrator of the build of the entire PR.

  In technical terms, CI job is a GenServer-like process which is related to a particular build (e.g. PR). The
  behaviour will initialize the process state, and then invoke various functions from the callback module. The
  callbacks are mostly similar to GenServer ones, with some build-specific additions.

  The callback module can start child jobs using the `start_job/3` function. The behaviour will monitor the lifecycle
  of these jobs, and notify the callback module when they terminate. The behaviour also takes care of the proper
  cleanup of all child jobs.

  The behaviour will also subscribe to notifications from `AircloakCI.RepoDataProvider`, and handle changes. If the
  related PR is no longer pending, the behaviour will terminate the process. If some new commits are pushed to the PR,
  the behaviour will stop all child jobs, and notify the callback module. Finally, the callback module will be notified
  if something else changes in the PR (e.g. status message).
  """
  use GenServer
  require Logger
  alias AircloakCI.{Github, LocalProject}

  @type state :: %{
    callback_mod: module,
    repo_data: Github.API.repo_data,
    source: source,
    project: LocalProject.t,
    data: any,
    jobs: jobs,
  }

  @type source :: Github.API.pull_request | Github.API.branch

  @opaque jobs :: %{job_name => pid}

  @type job_name :: any

  @type async_message_result ::
    {:noreply, state} |
    {:noreply, state, timeout | :hibernate} |
    {:stop, reason :: any, state}


  # -------------------------------------------------------------------
  # Behaviour
  # -------------------------------------------------------------------

  @doc "Invoked when the process is initializing."
  @callback init(any, state) ::
    {:ok, state} |
    {:ok, state, timeout | :hibernate} |
    :ignore |
    {:stop, reason :: any}

  @doc "Invoked to find the source object."
  @callback refresh_source(state) :: source | nil

  @doc "Invoked to create the local project instance."
  @callback create_project(state) :: LocalProject.t

  @doc """
  Invoked when there's a change in the PR.

  This callback is not invoked if some additional commits are pushed. In this case, `handle_restart/1` will be invoked.
  """
  @callback handle_source_change(state) :: async_message_result

  @doc "Invoked if some new commits are pushed to the PR."
  @callback handle_restart(state) :: async_message_result

  @doc "Invoked if a child job terminated with the reason `:normal`."
  @callback handle_job_succeeded(job_name, state) :: async_message_result

  @doc "Invoked if a child job terminated with a non-normal reason."
  @callback handle_job_failed(job_name, reason :: any, state) :: async_message_result

  @doc "Invoked to handle a synchronous request issued by `call/3`."
  @callback handle_call(request :: any, {pid, tag :: term}, state) ::
    {:reply, reply, state} |
    {:reply, reply, state, timeout | :hibernate} |
    {:noreply, state} |
    {:noreply, state, timeout | :hibernate} |
    {:stop, reason, reply, state} |
    {:stop, reason, state} when reply: term, reason: term

  @doc "Invoked to handle a plain message sent to this process."
  @callback handle_info(message :: any, state) :: async_message_result


  # -------------------------------------------------------------------
  # API Functions
  # -------------------------------------------------------------------

  @doc "Starts a job runner related to the given pull request."
  @spec start_link(module, source, Github.API.repo_data, any, GenServer.options) :: GenServer.on_start
  def start_link(callback_mod, source, repo_data, arg, gen_server_opts \\ []), do:
    GenServer.start_link(__MODULE__, {callback_mod, source, repo_data, arg}, gen_server_opts)

  @doc "Makes a synchronous request to the given job runner."
  @spec call(GenServer.server, any, pos_integer | :infinity) :: any
  def call(server, request, timeout \\ :timer.seconds(5)), do:
    GenServer.call(server, request, timeout)

  @doc """
  Starts the job as a child of the job runner process.

  The provided lambda should either return `{:ok, pid}`, or `:ignore` (in which case the job is not started).
  The lambda should start the child process directly under the caller process.
  """
  @spec start_job(state, any, (() -> {:ok, pid})) :: state
  def start_job(state, name, starter_fun) do
    :error = Map.fetch(state.jobs, name)
    case starter_fun.() do
      {:ok, new_job} -> put_in(state.jobs[name], new_job)
      :ignore -> state
    end
  end

  @doc "Terminates all currently running child jobs."
  @spec terminate_all_jobs(state) :: state
  def terminate_all_jobs(state) do
    pids = Map.values(state.jobs)
    Enum.each(pids, &Process.exit(&1, :shutdown))
    Enum.each(pids, &await_shutdown_or_kill/1)
    %{state | jobs: %{}}
  end

  @doc "Returns the list of currently running jobs."
  @spec running_jobs(state) :: [job_name]
  def running_jobs(state), do:
    Map.keys(state.jobs)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({callback_mod, source, repo_data, arg}) do
    Process.flag(:trap_exit, true)
    AircloakCI.RepoDataProvider.subscribe()

    %{
      callback_mod: callback_mod,
      repo_data: repo_data,
      source: source,
      project: nil,
      data: nil,
      jobs: %{}
    }
    |> update_project()
    |> invoke_callback(:init, [arg])
  end

  @impl GenServer
  def handle_call(request, from, state), do:
    invoke_callback(state, :handle_call, [request, from])

  @impl GenServer
  def handle_info({:repo_data, repo_data}, state) do
    state = %{state | repo_data: repo_data}
    case invoke_callback(state, :refresh_source, []) do
      nil ->
        Logger.info("shutting down job runner `#{__MODULE__}` for `#{LocalProject.name(state.project)}`")
        {:stop, :shutdown, state}
      new_source ->
        update_source(state, new_source)
    end
  end
  def handle_info({:EXIT, pid, reason} = exit_message, state) do
    case Enum.find(state.jobs, &match?({_name, ^pid}, &1)) do
      {name, ^pid} ->
        new_state = update_in(state.jobs, &Map.delete(&1, name))
        case reason do
          :normal -> invoke_callback(new_state, :handle_job_succeeded, [name])
          _other -> invoke_callback(new_state, :handle_job_failed, [name, reason])
        end

      nil -> invoke_callback(state, :handle_info, [exit_message])
    end
  end
  def handle_info(other, state), do:
    invoke_callback(state, :handle_info, [other])

  @impl GenServer
  def terminate(reason, state) do
    Logger.info([
      "job runner ", inspect(state.callback_mod), " for ", LocalProject.name(state.project),
      " terminating: ", Exception.format_exit(reason)
    ])
    terminate_all_jobs(state)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_project(state), do:
    %{state | project: invoke_callback(state, :create_project, [])}

  defp update_source(state, new_source) do
    new_state = update_project(%{state | source: new_source})
    cond do
      new_state.project.desired_sha != state.project.desired_sha ->
        new_state |> terminate_all_jobs() |> invoke_callback(:handle_restart, [])

      new_state.source != state.source ->
        invoke_callback(state, :handle_source_change, [])

      true ->
        {:noreply, state}
    end
  end

  defp await_shutdown_or_kill(pid) do
    receive do
      {:EXIT, ^pid, _reason} -> :ok
    after :timer.seconds(5) ->
      Process.exit(pid, :kill)
      receive do {:EXIT, ^pid, _reason} -> :ok end
    end
  end

  defp invoke_callback(state, fun, args), do:
    apply(state.callback_mod, fun, args ++ [state])

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts, behaviour_mod: __MODULE__] do
      require Logger
      @behaviour behaviour_mod

      @impl behaviour_mod
      def handle_source_change(state), do:
        {:noreply, state}

      @impl behaviour_mod
      def handle_job_succeeded(job_name, state) do
        Logger.info("job #{job_name} finished")
        {:noreply, state}
      end

      @impl behaviour_mod
      def handle_job_failed(job_name, crash_reason, state) do
        Logger.error("job #{job_name} failed: #{Exception.format_exit(crash_reason)}")
        {:noreply, state}
      end

      @impl behaviour_mod
      def handle_restart(state), do:
        {:stop, :normal, state}

      @impl behaviour_mod
      def handle_call(request, from, state), do:
        raise "handle_call/3 not implemented in #{inspect(__MODULE__)}"

      @impl behaviour_mod
      def handle_info(msg, state) do
        proc =
          case Process.info(self(), :registered_name) do
            {_, []}   -> self()
            {_, name} -> name
          end
        :error_logger.error_msg('~p ~p received unexpected message in handle_info/2: ~p~n',
                                [__MODULE__, proc, msg])
        {:noreply, state}
      end

      defoverridable behaviour_mod

      @doc false
      def child_spec(_arg), do:
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, []},
          restart: unquote(Keyword.get(opts, :restart, :permanent)),
          shutdown: 5000,
          type: :worker
        }
      defoverridable child_spec: 1
    end
  end
end
