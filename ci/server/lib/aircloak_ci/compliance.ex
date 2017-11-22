defmodule AircloakCI.Compliance do
  @moduledoc "Compliance job runner."

  alias AircloakCI.Build


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs the compliance job."
  @spec run(Build.t) :: :ok | :error
  def run(build) do
    with \
      {:ok, build} = Build.initialize(build),
      :ok <- run_phase(build, "cloak build", "ci/run.sh build_cloak", timeout: :timer.minutes(10)),
      :ok <- run_phase(build, "compliance", "ci/run.sh cloak_compliance", timeout: :timer.minutes(10))
    do
      :ok
    else
      {:error, reason} ->
        Build.log(build, "error: #{reason}")
        :error
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_phase(build, title, cmd, opts) do
    Build.log(build, "starting #{title}")
    Build.cmd(build, cmd, opts)
  end
end
