defmodule AircloakCI.JobRunner.Branch do
  @moduledoc "Common implementations of callbacks for branch-oriented job runners."

  @doc false
  defmacro __using__(opts) do
    quote do
      use AircloakCI.JobRunner, unquote(opts)
      alias AircloakCI.LocalProject

      @impl AircloakCI.JobRunner
      def create_project(state), do:
        LocalProject.for_branch(state.source)

      @impl AircloakCI.JobRunner
      def refresh_source(state), do:
        Enum.find(state.repo_data.branches, &(&1.name == state.source.name && &1.repo == state.source.repo))
    end
  end
end
