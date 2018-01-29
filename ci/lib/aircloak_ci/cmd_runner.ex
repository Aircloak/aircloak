defmodule AircloakCI.CmdRunner do
  @moduledoc """
  Runner of OS commands.

  This module makes it possible to invoke an OS command. The main difference from `System.cmd/3` is that this module
  ensures that a command is terminated if the starter process terminates. This holds even for the cases when the entire
  BEAM is taken down. This is achieved with the help of the [erlexec](https://github.com/saleyn/erlexec) library.
  """

  use GenServer, start: {__MODULE__, :start_link, []}, restart: :temporary
  require Logger

  @type run_option ::
    {:timeout, pos_integer | :infinity} |
    {:kill_timeout, pos_integer} |
    {:cd, String.t}

  @type logger :: ((iodata) -> any)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs the command and waits for it to finish."
  @spec run(String.t, [{:logger, logger} | run_option]) :: :ok | {:error, String.t}
  def run(cmd, opts \\ []) do
    {:ok, runner} = AircloakCI.CmdRunner.Supervisor.start_runner(self())
    GenServer.call(runner, {:run, cmd, normalize_opts(opts)}, :infinity)
  end

  @doc "Runs the command, waits for it to finish, and returns the command output."
  @spec run_with_output(String.t, [run_option]) :: {:ok, String.t} | {:error, String.t}
  def run_with_output(cmd, opts \\ []) do
    {:ok, runner} = AircloakCI.CmdRunner.Supervisor.start_runner(self())
    GenServer.call(runner, {:run_with_output, cmd, normalize_opts(opts)}, :infinity)
  end

  @doc "Runs the command, waits for it to finish, and returns the command output. Raises if the command fails."
  @spec run_with_output!(String.t, [run_option]) :: String.t
  def run_with_output!(cmd, opts \\ []) do
    {:ok, output} = run_with_output(cmd, opts)
    output
  end

  @doc "Logger which appends outputs to the given file."
  @spec file_logger(String.t) :: logger
  def file_logger(path), do:
    fn(output) ->
      if Application.get_env(:aircloak_ci, :cmd_runner, [])[:console_out] == true, do: IO.write(output)
      File.write(path, output, [:append])
    end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(owner) do
    Process.flag(:trap_exit, true)
    {:ok, %{owner_mref: Process.monitor(owner)}}
  end

  @impl GenServer
  def handle_call({:run, cmd, opts}, from, state) do
    {:noreply, start_cmd(state, cmd, from, [return_output: false] ++ opts)}
  end
  def handle_call({:run_with_output, cmd, opts}, from, state) do
    {:noreply, start_cmd(state, cmd, from, [return_output: true] ++ opts)}
  end

  @impl GenServer
  def handle_info({:EXIT, pid, reason}, %{pid: pid} = state) do
    GenServer.reply(state.from, result(state, reason))
    {:stop, :shutdown, nil}
  end
  def handle_info(:timeout, state) do
    GenServer.reply(state.from, {:error, "timeout running `#{state.cmd}`"})
    {:stop, :shutdown, state}
  end
  def handle_info({:DOWN, owner_mref, :process, _pid, _reason}, %{owner_mref: owner_mref} = state) do
    exec_mod().stop(state.pid)
    {:stop, :shutdown, nil}
  end
  def handle_info({:stdout, os_pid, output}, %{os_pid: os_pid} = state), do:
    {:noreply, update_in(state.output, &[&1, output])}
  def handle_info({:stderr, os_pid, output}, %{os_pid: os_pid} = state), do:
    {:noreply, update_in(state.output, &[&1, output])}
  def handle_info(other, state), do:
    super(other, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_opts(opts), do:
    Enum.map(
      opts,
      fn
        {key, value} when key in [:cd] -> {key, to_charlist(value)}
        {:kill_timeout, timeout} -> Float.ceil(timeout / 1000.0)
        other -> other
      end
    )

  defp result(%{return_output?: false}, :normal), do: :ok
  defp result(%{return_output?: true} = state, :normal), do: {:ok, to_string(state.output)}
  defp result(%{return_output?: false} = state, {:exit_status, _status}), do: {:error, "error running `#{state.cmd}`"}
  defp result(%{return_output?: true} = state, {:exit_status, _status}), do: {:error, to_string(state.output)}

  defp start_cmd(state, cmd, from, opts) do
    return_output? = Keyword.fetch!(opts, :return_output)

    # The default timeout of 1 minutes is chosen as a value which is neither "too long" nor "too short". A one minute
    # timeout should suffice for "short-running" commands, even if the system is a bit overloaded, while at the same
    # time it ensures that a command eventually stops within a "reasonable" time.
    case Keyword.get(opts, :timeout, :timer.minutes(1)) do
      :infinity -> :ok
      timeout when is_integer(timeout) and timeout > 0 ->
        Process.send_after(self(), :timeout, timeout)
    end

    passthrough_opts = Keyword.take(opts, [:kill_timeout, :cd])

    output_opts =
      if return_output? do
        [:stdout, :stderr]
      else
        print_output = fn(_stdout_or_err, _os_pid, output) -> log_output(output, opts) end
        [stdout: print_output, stderr: print_output]
      end

    log_output("aircloak_ci: `#{cmd}`\n", opts)
    {:ok, pid, os_pid} = exec_mod().run_link(to_charlist(cmd), output_opts ++ passthrough_opts)
    {pid, os_pid}

    Map.merge(state, %{cmd: cmd, from: from, pid: pid, os_pid: os_pid, output: [], return_output?: return_output?})
  end

  defp exec_mod(), do: Application.get_env(:aircloak_ci, :exec_mod, :exec)

  defp log_output(output, opts) do
    case Keyword.fetch(opts, :logger) do
      :error -> :ok
      {:ok, logger} -> logger.(output)
    end
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(owner), do:
    GenServer.start_link(__MODULE__, owner)
end
