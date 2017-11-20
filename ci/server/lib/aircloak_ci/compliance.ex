defmodule AircloakCI.Compliance do
  @moduledoc "Compliance job runner."

  alias AircloakCI.Build


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs the compliance job."
  @spec run(AircloakCI.Github.pull_request) :: :ok | {:error, String.t}
  def run(pr) do
    with \
      :ok <- Build.initialize(pr),
      :ok <- Build.cmd(pr, "ci/run.sh build_cloak", timeout: :timer.minutes(10)),
    do: Build.cmd(pr, "ci/run.sh cloak_compliance", timeout: :timer.minutes(10))
  end
end
