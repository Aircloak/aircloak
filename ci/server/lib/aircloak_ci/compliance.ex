defmodule AircloakCI.Compliance do
  @moduledoc "Compliance job runner."

  alias AircloakCI.Build


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs the compliance job."
  @spec run(AircloakCI.Github.API.pull_request) :: :ok | :error
  def run(pr) do
    with \
      :ok <- Build.initialize(pr),
      :ok <- run_phase(pr, "cloak build", "ci/run.sh build_cloak", timeout: :timer.minutes(10)),
      :ok <- run_phase(pr, "compliance", "ci/run.sh cloak_compliance", timeout: :timer.minutes(10))
    do
      :ok
    else
      {:error, reason} ->
        Build.log(pr, "error: #{reason}")
        :error
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_phase(pr, title, cmd, opts) do
    Build.log(pr, "starting #{title}")
    Build.cmd(pr, cmd, opts)
  end
end
