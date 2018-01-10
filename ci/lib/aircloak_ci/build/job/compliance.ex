  defmodule AircloakCI.Build.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.{Component, Job}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Invokes the compliance job."
  @spec run(Build.Server.state) :: Build.Server.state
  def run(build_state), do:
    Job.maybe_start(build_state, job_name(), &maybe_start_test/1)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp job_name(), do: "compliance"

  defp maybe_start_test(%{source: pr} = build_state) do
    case check_start_preconditions(build_state) do
      :ok -> start_test(self(), build_state)

      {:error, status} ->
        Build.Reporter.report_status(pr.repo, pr.sha, job_name(), pr.status_checks, :pending, status)
        build_state
    end
  end

  defp check_start_preconditions(build_state) do
    if LocalProject.forced?(build_state.project, job_name()) do
      :ok
    else
      with :ok <- verify_required_statuses(build_state), do: verify_approval(build_state)
    end
  end

  defp verify_required_statuses(build_state) do
    case \
      [
        "continuous-integration/aircloak/air_test",
        "continuous-integration/aircloak/cloak_test",
        "continuous-integration/aircloak/integration_tests_test",
        "continuous-integration/travis-ci/pr",
      ]
      |> Stream.map(&{&1, build_state.source.status_checks[&1][:status]})
      |> Stream.reject(&match?({_, :success}, &1))
      |> Enum.map(fn({status, _}) -> String.replace(status, ~r[^continuous\-integration\/], "") end)
    do
      [] -> :ok
      [pending_status | _] -> {:error, "waiting for #{pending_status} to succeed"}
    end
  end

  defp verify_approval(%{source: %{approved?: true}}), do: :ok
  defp verify_approval(%{source: %{approved?: false}}), do: {:error, "waiting for approval"}

  defp start_test(build_server, %{source: pr, project: project} = build_state), do:
    Build.Server.start_job(
      build_state,
      job_name(),
      fn -> Component.start_job(project, "cloak", :compliance, report_result: build_server, log_name: "compliance") end,
      report_status: {pr.repo, pr.sha}
    )
end
