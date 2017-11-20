defmodule AircloakCI.CmdRunner do
  @moduledoc """
  Runner of OS commands.

  This module makes it possible to invoke an OS command. The main difference from `System.cmd/3` is that this module
  ensures that a command is terminated if the starter process terminates. This holds even for the cases when the entire
  BEAM is taken down. This is achieved with the help of the [erlexec](https://github.com/saleyn/erlexec) library.
  """

  use GenServer, start: {__MODULE__, :start_link, []}, restart: :temporary
  require Logger

  @type opts :: [
    timeout: pos_integer | :infinity,
    kill_timeout: pos_integer,
    cd: String.t,
    logger: logger
  ]

  @type logger :: ((iodata) -> any)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs the command and waits for it to finish, raises on error."
  @spec run!(String.t, opts) :: :ok
  def run!(cmd, opts \\ []), do:
    :ok = run(cmd, opts)

  @doc "Runs the command and waits for it to finish."
  @spec run(String.t, opts) :: :ok | {:error, String.t}
  def run(cmd, opts \\ []) do
    {:ok, runner} = AircloakCI.CmdRunner.Supervisor.start_runner()
    GenServer.call(runner, {:run, cmd, normalize_opts(opts)}, :infinity)
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
  def init(nil) do
    Process.flag(:trap_exit, true)
    {:ok, nil}
  end

  @impl GenServer
  def handle_call({:run, cmd, opts}, from, nil) do
    case Keyword.get(opts, :timeout, :timer.seconds(5)) do
      :infinity -> :ok
      timeout when is_integer(timeout) and timeout > 0 ->
        Process.send_after(self(), :timeout, timeout)
    end

    {:noreply, %{cmd: cmd, from: from, pid: start_cmd(cmd, opts)}}
  end

  @impl GenServer
  def handle_info({:EXIT, pid, reason}, %{pid: pid} = state) do
    GenServer.reply(state.from, result(reason, state.cmd))
    {:stop, :shutdown, state}
  end
  def handle_info(:timeout, state) do
    GenServer.reply(state.from, {:error, "timeout running `#{state.cmd}`"})
    {:stop, :shutdown, state}
  end
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

  defp result(:normal, _cmd), do: :ok
  defp result({:exit_status, _status}, cmd), do: {:error, "error running `#{cmd}`"}

  defp start_cmd(cmd, opts) do
    print_output = fn(_stdout_or_err, _os_pid, output) -> log_output(output, opts) end

    {:ok, pid, _os_pid} = :exec.run_link(to_charlist(cmd),
      [stdout: print_output, stderr: print_output] ++ Keyword.take(opts, [:kill_timeout, :cd]))

    pid
  end

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
  def start_link(), do:
    GenServer.start_link(__MODULE__, nil)
end
