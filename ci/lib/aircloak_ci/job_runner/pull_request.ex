defmodule AircloakCI.JobRunner.PullRequest do
  @moduledoc "Common implementations of callbacks for PR-oriented job runners."

  @doc false
  defmacro __using__(opts) do
    quote do
      use AircloakCI.JobRunner, unquote(opts)
      alias AircloakCI.LocalProject

      @impl AircloakCI.JobRunner
      def create_project(state), do:
        LocalProject.for_pull_request(state.source)

      @impl AircloakCI.JobRunner
      def refresh_source(state), do:
        Enum.find(state.repo_data.pull_requests, &(&1.number == state.source.number))
    end
  end
end
