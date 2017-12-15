defmodule AircloakCI.Build.Component do
  @moduledoc """
  Executes jobs on the desired component.

  A component is a project in the repo, such as air, cloak, ...

  A job is the name under which the component exposes a set of commands to be executed.
  Examples of job names are `:compile`, `:standard_test`, `:compliance`.
  """

  alias AircloakCI.{Container, LocalProject}
  alias AircloakCI.Build.Job

  @type job :: :compile | :standard_test | :compliance


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
          Container.exec(container, LocalProject.commands(project, component, job), timeout: :timer.hours(1))
      end
    )

  defp with_container(project, component, job, fun), do:
    Container.with(script(project, component), LocalProject.log_file(project, log_name(component, job)), fun)

  defp prepare_for(container, job) do
    if job in [:standard_test, :compliance] do
      command_suffix = if job == :standard_test, do: "test", else: to_string(job)
      Container.invoke_script(container, "prepare_for_#{command_suffix} #{container.name}", timeout: :timer.minutes(1))
    else
      :ok
    end
  end

  defp script(project, component), do:
    project |> LocalProject.src_folder() |> Path.join("ci/scripts/#{component}.sh")

  defp log_name(component, :standard_test), do: "#{component}_test"
  defp log_name(component, job), do: "#{component}_#{job}"
end
