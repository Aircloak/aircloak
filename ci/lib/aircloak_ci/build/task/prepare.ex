defmodule AircloakCI.Build.Task.Prepare do
  @moduledoc "Preparation of the project source code."

  alias AircloakCI.{JobRunner, LocalProject}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Compiles all the components in the given project."
  @spec run(JobRunner.t, AircloakCI.Github.API.branch | nil, [delay: non_neg_integer]) :: JobRunner.t
  def run(%{project: project} = job_runner_state, base_branch, opts \\ []), do:
    JobRunner.start_task(job_runner_state, __MODULE__, fn -> initialize_repo(project, base_branch, opts) end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp initialize_repo(project, base_branch, opts) do
    case Keyword.fetch(opts, :delay) do
      :error -> :ok
      {:ok, delay} -> :timer.sleep(delay)
    end

    unless is_nil(base_branch) or LocalProject.initialized?(project), do:
      AircloakCI.Build.Branch.transfer_project(base_branch, project)

    :ok = LocalProject.update_code(project)
  end
end
