defmodule AircloakCI.Build.PullRequest do
  @moduledoc """
  This module powers the process responsible for the entire build of the single PR.

  The process will start various child jobs to initialize the repo and run different tests.
  """

  use AircloakCI.Build.Server, restart: :temporary
  require Logger
  alias AircloakCI.{Build, Github, LocalProject}
  alias AircloakCI.Build.Job


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given pull request is started."
  @spec ensure_started(Github.API.pull_request, Github.API.repo_data) :: pid
  def ensure_started(pr, repo_data) do
    case Build.Supervisor.start_build(__MODULE__, [pr, repo_data]) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end


  # -------------------------------------------------------------------
  # Build.Server callbacks
  # -------------------------------------------------------------------

  @impl Build.Server
  def build_source(pr_number, repo_data) do
    pr = Enum.find(repo_data.pull_requests, &(&1.number == pr_number))
    if is_nil(pr) do
      nil
    else
      %{
        source: pr,
        base_branch: Enum.find(repo_data.branches, &(&1.name == pr.target_branch)),
        project: LocalProject.for_pull_request(pr)
      }
    end
  end

  @impl Build.Server
  def init(nil, state) do
    :timer.send_interval(:timer.seconds(5), self(), :report_mergeable)
    {:ok, report_mergeable(%{state | data: %{mergeable_info: nil}})}
  end

  @impl Build.Server
  def handle_source_change(state) do
    state = maybe_start_ci(state)
    {:noreply, report_mergeable(state)}
  end

  @impl Build.Server
  def handle_job_succeeded("compile", state), do: {:noreply, maybe_start_ci(state)}
  def handle_job_succeeded(other, state), do: super(other, state)

  @impl Build.Server
  # Note: we'll start the CI even if the compilation failed. If the resulting errors occur again, they will be properly
  # reported to the author.
  def handle_job_failed("compile", _reason, state), do: {:noreply, maybe_start_ci(state)}
  def handle_job_failed(other, reason, state), do: super(other, reason, state)

  @impl Build.Server
  def handle_info(:report_mergeable, state), do: {:noreply, report_mergeable(state)}
  def handle_info(other, state), do: super(other, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {Build.Registry, {:pull_request, pr.number}}}

  defp maybe_start_ci(%{compiled?: false} = state), do: state
  defp maybe_start_ci(%{compiled?: true} = state) do
    if state.source.merge_state == :mergeable and state.source.merge_sha != nil,
      do: state |> Job.Compliance.run() |> Job.Test.run(),
      else: state
  end

  defp report_mergeable(state) do
    {status, message} =
      with :ok <- check_mergeable(state),
           :ok <- check_compiled(state),
           :ok <- check_required_statuses(state),
           :ok <- check_approved(state),
        do: {:success, "pull request can be merged"}

    if status == :success and not LocalProject.finished?(state.project, "report_mergeable") do
      merge_message = "Pull request can be merged #{AircloakCI.Emoji.happy()}"
      Github.comment_on_issue(state.source.repo.owner, state.source.repo.name, state.source.number, merge_message)
      LocalProject.mark_finished(state.project, "report_mergeable")
    end

    new_info = {state.source.sha, message, status}

    if new_info != state.data.mergeable_info do
      Github.put_status_check_state(
        state.source.repo.owner,
        state.source.repo.name,
        state.source.sha,
        "continuous-integration/aircloak/mergeable",
        message,
        status
      )

      put_in(state.data.mergeable_info, new_info)
    else
      state
    end
  end

  defp check_compiled(%{compiled?: true}), do: :ok
  defp check_compiled(%{compiled?: false}), do: {:pending, "compiling project"}

  defp check_mergeable(%{source: %{merge_state: :mergeable}}), do: :ok
  defp check_mergeable(%{source: %{merge_state: :unknown}}), do: {:pending, "computing mergeability"}
  defp check_mergeable(%{source: %{merge_state: :conflicting}}), do: {:error, "there are merge conflicts"}

  defp check_approved(%{source: %{approved?: true}}), do: :ok
  defp check_approved(%{source: %{approved?: false}}), do: {:pending, "waiting for approval"}

  defp check_required_statuses(state) do
    statuses =
      state
      |> required_statuses()
      |> Enum.map(&{Map.get(state.source.status_checks, &1, %{status: :pending}).status, &1})

    cond do
      Enum.all?(statuses, &match?({:success, _}, &1)) -> :ok

      Enum.any?(statuses, fn {status, _} -> status in [:error, :failure] end) ->
        {:error, "some checks have failed"}

      true ->
        [{_, name} | _] = Enum.reject(statuses, fn {status, _} -> status in [:success, :error, :failure] end)
        {:pending, "waiting for #{String.replace(name, ~r[^continuous\-integration\/], "")}"}
    end
  end

  defp required_statuses(state), do:
    state.project
    |> LocalProject.components()
    |> Enum.map(&"continuous-integration/aircloak/#{&1}_test")
    |> Enum.concat(["continuous-integration/aircloak/compliance"])


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data), do:
    Build.Server.start_link(__MODULE__, :pull_request, pr.number, repo_data, nil, name: name(pr))
end
