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
            LocalProject.mark_finished(project, "#{component_mod.name()}_compile")

          {:error, component_mod} ->
            LocalProject.log(project, "main", "error compiling component #{component_mod.name()}")
        end
      )

  defp component_modules(), do:
    [Component.Cloak]

  defp compile(project, component_mod) do
    component_name = component_mod.name()
    if LocalProject.finished?(project, "#{component_name}_compile") do
      :ok
    else
      log_name = "#{component_name}_compile"
      Job.run_queued(:compile, project,
        fn ->
          with {:error, reason} <- component_mod.compile(project, component_name, log_name) do
            LocalProject.log(project, log_name, reason)
            :error
          end
        end,
        log_name: log_name
      )
    end
  end
end
