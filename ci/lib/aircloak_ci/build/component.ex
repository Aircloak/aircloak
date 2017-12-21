defmodule AircloakCI.Build.Component do
  @moduledoc """
  Executes jobs on the desired component.

  A component is a project in the repo, such as air, cloak, ...

  A job is the name under which the component exposes a set of commands to be executed.
  Examples of job names are `:compile`, `:test`, `:compliance`.
  """

  alias AircloakCI.{Container, LocalProject}
  alias AircloakCI.Build.Job

  @type job :: :compile | :test | :compliance


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts a job on the desired component."
  @spec start_job(LocalProject.t, String.t, job, Job.run_queued_opts) :: :ok | {:error, String.t}
  def start_job(project, component, job, opts \\ []) do
    with :ok <- build_image(project, component), do:
      Job.run_queued(job, project, fn -> run_job(project, component, job) end,
        Keyword.merge([log_name: log_name(component, job)], opts)
      )
  end


  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp build_image(project, component) do
    if Container.built?(script(project, component)) do
      :ok
    else
      log_name = "#{component}_docker_build"
      Job.run_queued(:docker_build, project,
        fn -> Container.build(script(project, component), LocalProject.log_file(project, log_name)) end,
        log_name: log_name
      )
    end
  end

  defp run_job(project, component, job), do:
    with_container(project, component, job,
      fn(container) ->
        with :ok <- prepare_for(container, job), do:
          run_commands(container, LocalProject.commands(project, component, job))
      end
    )

  defp run_commands(container, commands) when is_list(commands), do:
      run_commands(container, {:sequence, commands})
  defp run_commands(container, {:sequence, commands}), do:
    commands
    |> Stream.map(&run_commands(container, &1))
    |> ok_or_first_error()
  defp run_commands(container, {:parallel, commands}), do:
    commands
    |> Task.async_stream(&run_commands(container, &1), ordered: false, timeout: :infinity)
    |> Stream.map(fn {:ok, task_result} -> task_result end)
    |> ok_or_first_error()
  defp run_commands(container, command) when is_binary(command) do
    if Application.get_env(:aircloak_ci, :simulate_commands, false) do
      IO.puts "simulating execution of `#{command}`"
      :timer.sleep(1000)
      :ok
    else
      Container.exec(container, [command], timeout: :timer.hours(1))
    end
  end

  defp ok_or_first_error(results) do
    case \
      results
      |> Stream.drop_while(&(&1 == :ok))
      |> Enum.take(1)
    do
      [] -> :ok
      [error] -> error
    end
  end

  defp with_container(project, component, job, fun), do:
    Container.with(script(project, component), LocalProject.log_file(project, log_name(component, job)), fun)

  defp prepare_for(container, job) do
    if job in [:test, :compliance] do
      Container.invoke_script(container, "prepare_for_#{job} #{container.name}", timeout: :timer.minutes(1))
    else
      :ok
    end
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

  defp log_name(component, job), do: "#{component}_#{job}"
end
