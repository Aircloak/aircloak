defmodule AircloakCI.Github.StatusPoller do
  @moduledoc "Process which periodically retrieves pending pull requests from Github."

  use Aircloak.ChildSpec.Task
  alias AircloakCI.Github
  require Logger


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp loop() do
    try do
      AircloakCI.Builder.Server.handle_pending_prs(Github.pending_pull_requests("aircloak", "aircloak"))
    catch type, error ->
      Logger.error(Exception.format(type, error, System.stacktrace()))
    end

    :timer.sleep(:timer.seconds(5))
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do:
    Task.start_link(&loop/0)
end
