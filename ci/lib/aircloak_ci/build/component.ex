defmodule AircloakCI.Build.Component do
  @moduledoc """
  Executes jobs on the desired component.

  A component is a project in the repo, such as air, cloak, ...

  A job is the name under which the component exposes a set of commands to be executed.
  Examples of job names are `:compile`, `:test`, `:compliance`.
  """

  alias AircloakCI.{CmdRunner, Container, LocalProject}
  alias AircloakCI.Build.Job

  @type job :: :compile | :test | :compliance | :system_test

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts a job on the desired component."
  @spec start_job(LocalProject.t(), String.t(), job, Job.run_queued_opts()) :: :ok | {:error, String.t()}
  def start_job(project, component, job, opts \\ []) do
    with :ok <- build_image(project, component) do
      opts = Keyword.merge([log_name: "#{component}_#{job}"], opts)
      Job.run_queued(job, project, fn -> run_job(project, component, job, opts) end, opts)
    end
  end

  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp build_image(project, component) do
    if Container.built?(script(project, component)) do
      :ok
    else
      log_name = "#{component}_docker_build"

      Job.run_queued(
        :docker_build,
        project,
        fn ->
          Container.build(script(project, component), LocalProject.log_file(project, log_name))
        end,
        log_name: log_name
      )
    end
  end

  defp run_job(project, component, job, opts),
    do: AircloakCI.Queue.exec(:job, fn -> do_run_job(project, component, job, opts) end)

  defp do_run_job(project, component, job, opts),
    do:
      with_container(project, component, opts, fn container ->
        with :ok <- prepare_for(container, job) do
          commands = LocalProject.commands(project, component, job)
          {result, outputs} = run_commands(project, component, job, container, commands)
          # dump all outputs to the job log file
          File.write(container.log_file, ["\n", outputs, "\n"], [:append])
          result
        end
      end)

  defp run_commands(project, component, job, container, commands) when is_list(commands),
    do: run_commands(project, component, job, container, {:sequence, commands})

  defp run_commands(project, component, job, container, {:sequence, commands}),
    do:
      commands
      |> Stream.map(&run_commands(project, component, job, container, &1))
      |> collect_results_from_sequence()

  defp run_commands(project, component, job, container, {:parallel, commands}),
    do:
      commands
      |> Task.async_stream(
        &run_commands(project, component, job, container, &1),
        ordered: false,
        timeout: :infinity
      )
      |> Stream.map(fn {:ok, task_result} -> task_result end)
      |> collect_results_from_parallel_commands()

  defp run_commands(project, component, job, container, command) when is_binary(command) do
    File.write(container.log_file, "started `#{command}`\n", [:append])

    command =
      if Application.get_env(:aircloak_ci, :simulate_commands, false) do
        "echo simulating #{command}"
      else
        command
      end

    # We'll log to the temporary unique file. This allows us to deinterlace log outputs later
    log_name = "#{component}_#{job}_#{Base.url_encode64(:crypto.strong_rand_bytes(16), padding: false)}"

    cmd_log_file = LocalProject.log_file(project, log_name)
    File.write(cmd_log_file, "")

    try do
      logger = CmdRunner.file_logger(cmd_log_file)
      start = :erlang.monotonic_time(:second)
      result = Container.exec(container, [command], timeout: :timer.hours(1), logger: logger)
      diff_sec = :erlang.monotonic_time(:second) - start

      # return result of command execution, and the output from the file
      {result, File.read!(cmd_log_file) <> "=> #{diff_sec} sec\n"}
    after
      # now we can safely delete the file
      File.rm(cmd_log_file)
    end
  end

  defp collect_results_from_sequence(commands_stream) do
    # In a sequence, we're stopping on first error, and return the outputs in the proper order.
    {result, outputs} =
      Enum.reduce_while(commands_stream, {:ok, []}, fn
        {:ok, output}, {:ok, outputs} -> {:cont, {:ok, [output | outputs]}}
        {{:error, _} = error, output}, {:ok, outputs} -> {:halt, {error, [output | outputs]}}
      end)

    {result, to_string(Enum.intersperse(Enum.reverse(outputs), "\n"))}
  end

  defp collect_results_from_parallel_commands(commands_stream) do
    # With parallel commands, we'll wait for all of them to finish. Since they are parallel, an error in one component
    # shouldn't affect the other, so we want to collect all possible errors.
    # We'll also do some heuristic reordering to improve UX. We'll put successes on top, ordering by the output length.
    # Errors are at the end, ordered by the descending output length. Thus, the shortest error output should be last,
    # and therefore its more likely to be included in the tail sent to the author.
    {cmd_results, outputs} =
      commands_stream
      |> Enum.sort_by(fn
        {:ok, output} -> {0, byte_size(output)}
        {{:error, _}, output} -> {1, -byte_size(output)}
      end)
      |> Enum.unzip()

    result =
      case Enum.filter(cmd_results, &match?({:error, _}, &1)) do
        [] ->
          :ok

        errors ->
          {:error, "\n" <> (errors |> Enum.map(fn {:error, reason} -> reason end) |> Enum.join("\n"))}
      end

    {result, to_string(Enum.intersperse(outputs, "\n"))}
  end

  defp with_container(project, component, opts, fun),
    do:
      Container.with(
        script(project, component),
        LocalProject.log_file(project, Keyword.fetch!(opts, :log_name)),
        fun
      )

  defp prepare_for(container, job) do
    # Note that we're ignoring the script outcome, meaning that we push forward even if preparation fails.
    # This is a quick fix for older builds which don't explicitly handle `prepare_*` argument. A concrete example
    # is `prepare_compile` which has been introduced, but it's not supported by older builds, such as previous release
    # branches.
    if job in [:compile, :test, :compliance, :system_test],
      do: Container.invoke_script(container, "prepare_for_#{job} #{container.name}", timeout: :timer.hours(1))

    :ok
  end

  defp script(project, component) do
    [path] =
      Enum.filter(
        [
          Path.join([LocalProject.src_folder(project) | ~w(#{component} ci container.sh)]),
          # supported for legacy reasons
          Path.join([LocalProject.src_folder(project) | ~w(ci scripts #{component}.sh)])
        ],
        &File.exists?/1
      )

    path
  end
end
