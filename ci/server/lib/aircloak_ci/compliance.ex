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
      :ok <- Build.truncate_logs(build),
      :ok <- Build.set_status(build, :started),
      :ok <- run_phase(build, "cloak build", &Build.compile/1),
      :ok <- run_phase(build, "compliance", &Build.cmd(&1, "ci/run.sh cloak_compliance", timeout: :timer.minutes(10)))
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

  defp run_phase(build, title, fun) do
    Build.log(build, "starting #{title}")
    fun.(build)
  end
end
