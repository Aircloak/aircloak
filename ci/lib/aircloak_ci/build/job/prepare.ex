defmodule AircloakCI.Build.Job.Prepare do
  @moduledoc "Preparation of the project source code."

  alias AircloakCI.{Build, LocalProject}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the source code for the given project."
  @spec start(Build.Server.t(), delay: non_neg_integer) :: Build.Server.t()
  def start(%{project: project, base_branch: base_branch} = build_state, opts \\ []),
    do:
      Build.Server.start_job(build_state, "prepare", fn ->
        initialize_repo(project, base_branch, opts)
      end)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp initialize_repo(project, base_branch, opts) do
    case Keyword.fetch(opts, :delay) do
      :error -> :ok
      {:ok, delay} -> :timer.sleep(delay)
    end

    unless is_nil(base_branch) or LocalProject.initialized?(project),
      do: AircloakCI.Build.Branch.transfer_project(base_branch, project)

    :ok = LocalProject.update_code(project)
  end
end
