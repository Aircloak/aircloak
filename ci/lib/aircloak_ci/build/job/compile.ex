defmodule AircloakCI.Build.Job.Compile do
  @moduledoc "Compilation of all components in a local project."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.Component


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
    components()
    # using infinity, since timeout is enforced in each component compilation task
    |> Task.async_stream(&{build_image_and_compile(project, &1), &1}, ordered: true, timeout: :infinity)
    |> Stream.map(fn {:ok, result} -> result end)
    |> Enum.each(
        fn
          {:ok, component} ->
            # setting the status here, to avoid concurrency issues
            LocalProject.mark_finished(project, "#{component}_compile")

          {:error, component} ->
            LocalProject.log(project, "main", "error compiling component #{component}")
        end
      )

  defp components(), do:
    ["cloak"]

  defp build_image_and_compile(project, component) do
    job_name = "#{component}_compile"
    if LocalProject.finished?(project, job_name) and not LocalProject.forced?(project, job_name) do
      :ok
    else
      Component.start_job(project, component, :compile)
    end
  end
end
