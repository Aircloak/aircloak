defmodule AircloakCI.Build.Task.Compile do
  @moduledoc "Compilation of all components in a local project."

  alias AircloakCI.{LocalProject, Queue}
  alias AircloakCI.Build.Component


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
  @spec run(LocalProject.t) :: :ok | {:error, String.t}
  def run(project) do
    failed_components =
      component_modules()
      |> Enum.map(&Task.async(fn -> compile(project, &1) end))
      # using infinity, since timeout is enforced in each component compilation task
      |> Stream.map(&Task.await(&1, :infinity))
      |> Stream.zip(component_modules())
      |> Stream.map(
          fn
            {:ok, component_mod} ->
              # setting the status here, to avoid concurrency issues
              LocalProject.mark_compiled(project, component_mod.name())
              nil

            {:error, component_mod} ->
              component_mod.name
          end
        )
      |> Enum.reject(&is_nil/1)

    if Enum.empty?(failed_components) do
      :ok
    else
      error = "error compiling components #{Enum.join(failed_components, ", ")}"
      LocalProject.log(project, "main", error)
      {:error, error}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp component_modules(), do:
    [Component.CI, Component.Cloak]

  defp compile(project, component_mod) do
    component_name = component_mod.name()
    if LocalProject.compiled?(project, component_name) do
      :ok
    else
      Queue.exec(
        :compile,
        fn ->
          LocalProject.log_start_stop(
            project,
            "compiling #{component_name} in #{LocalProject.name(project)}",
            fn ->
              log_name = "#{component_name}_compile"
              LocalProject.truncate_log(project, log_name)
              with {:error, reason} <- component_mod.compile(project, component_name, log_name) do
                LocalProject.log(project, log_name, reason)
                :error
              end
            end
          )
        end
      )
    end
  end
end
