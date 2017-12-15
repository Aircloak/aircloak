defmodule AircloakCI.Build.Job.Compile do
  @moduledoc "Compilation of all components in a local project."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.{Component, Job}


  # -------------------------------------------------------------------
  # Behaviour
  # -------------------------------------------------------------------

  @doc "Invoked to get the component name."
  @callback name() :: String.t

  @doc "Invoked to compile the component."
  @callback compile(LocalProject.t, name :: String.t, log_name :: String.t) :: :ok | {:error, String.t}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Compiles all the components in the given project."
  @spec run(Build.Server.state) :: Build.Server.state
  def run(%{project: project} = build_state), do:
    Build.Server.start_job(build_state, "compile", fn -> compile_project(project) end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_project(project), do:
    component_modules()
    # using infinity, since timeout is enforced in each component compilation task
    |> Task.async_stream(&compile(project, &1), ordered: true, timeout: :infinity)
    |> Stream.map(fn {:ok, result} -> result end)
    |> Stream.zip(component_modules())
    |> Enum.each(
        fn
          {:ok, component_mod} ->
            # setting the status here, to avoid concurrency issues
            LocalProject.mark_finished(project, job_name(component_mod.name()))

          {:error, component_mod} ->
            LocalProject.log(project, "main", "error compiling component #{component_mod.name()}")
        end
      )

  defp component_modules(), do:
    [Component.Cloak]

  defp compile(project, component_mod) do
    component_name = component_mod.name()
    job_name = job_name(component_name)
    if LocalProject.finished?(project, job_name) and not LocalProject.forced?(project, job_name) do
      :ok
    else
      Job.run_queued(:compile, project,
        fn -> component_mod.compile(project, component_name, job_name) end,
        log_name: job_name
      )
    end
  end

  defp job_name(component_name), do:
    "#{component_name}_compile"
end
