defmodule AircloakCI.BuildCase do
  @moduledoc "ExUnit case template for testing a CI build of a pull request or a branch."

  use ExUnit.CaseTemplate
  alias AircloakCI.TestGithubAPI

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Asserts that a status has been set on the given pull request."
  defmacro assert_posted_status(pr, context, pattern) do
    quote do
      pr = unquote(pr)
      sha = pr.sha
      context = "continuous-integration/aircloak/#{unquote(context)}"

      wait_for_jobs_to_finish(pr)

      assert_receive {:posted_status, unquote(pattern) = %{sha: ^sha, context: ^context}}, :timer.seconds(2)
    end
  end

  @doc "Asserts that a comment has been posted to the given pull request."
  defmacro assert_pr_comment(pr, pattern) do
    quote do
      pr = unquote(pr)
      wait_for_jobs_to_finish(pr)
      pr_number = unquote(pr).number
      assert_receive {:commented_on_issue, unquote(pattern) = %{issue_number: ^pr_number}}, :timer.seconds(2)
    end
  end

  @doc "Returns the names of the successful jobs."
  def successful_jobs(pr) do
    pr
    |> job_outcomes()
    |> Enum.filter(fn({_name, outcome}) -> outcome == :ok end)
    |> Enum.map(fn({name, _outcome}) -> name end)
    |> Enum.sort()
  end

  @doc "Returns boolean indicating whether the given job succeeded."
  def successful_job?(pr, job_name), do: pr |> successful_jobs() |> Enum.member?(job_name)

  @doc "Returns the names of the failed jobs."
  def failed_jobs(pr) do
    pr
    |> job_outcomes()
    |> Enum.filter(fn({_name, outcome}) -> outcome != :ok end)
    |> Enum.map(fn({name, _outcome}) -> name end)
    |> Enum.sort()
  end

  @doc "Creates a new unique pull request."
  def pr(data \\ []) do
    pr_number = Keyword.get(data, :number, :erlang.unique_integer([:positive]))
    defaults =
      %{
        repo: repo(),
        number: pr_number,
        title: "test pr #{pr_number}",
        source_branch: "feature_#{pr_number}",
        target_branch: "master",
        sha: new_sha(),
        merge_sha: new_sha(),
        merge_state: :mergeable,
        approved?: false,
        status_checks: []
      }
    Map.merge(defaults, Map.new(data))
  end

  @doc "Adds a pull request to the repo data structure."
  def add_pr(repo_data, pr), do: update_in(repo_data.pull_requests, &[pr | &1])

  @doc "Updates the PR data, and notifies the corresponding build server."
  def update_pr_data(repo_data, pr, updater) do
    new_pr = updater.(pr)
    new_repo_data = update_pr(repo_data, pr, fn(_) -> new_pr end)
    update_repo_data(new_pr, new_repo_data)
    {new_pr, new_repo_data}
  end

  @doc "Generates the new unique SHA."
  def new_sha(), do: Base.encode16(:crypto.strong_rand_bytes(20))

  @doc "Waits until all the jobs on the PR server have finished"
  def wait_for_jobs_to_finish(pr) do
    fn ->
      :timer.sleep(10)
      AircloakCI.Build.Server.running_jobs(pr_state(pr))
    end
    |> Stream.repeatedly()
    |> Stream.drop_while(&(not Enum.empty?(&1)))
    |> Stream.take(1)
    |> Stream.run()
  end

  @doc "Causes a pending command to fail."
  def fail_on_container_command(pr, component, cmd_regex) do
    cmd_prefix = "\.test_data/data/cache/builds/pr-#{pr.number}/src/#{component}/ci/container.sh"
    regex = Regex.compile!("^#{cmd_prefix}.*#{cmd_regex}$")

    AircloakCI.TestExec.add_exec_handler(fn(cmd) ->
      if to_string(cmd) =~ regex, do: exit({:exit_status, 1}), else: nil
    end)
  end


  # -------------------------------------------------------------------
  # ExUnit.CaseTemplate definition
  # -------------------------------------------------------------------

  using do
    quote do
      import unquote(__MODULE__)
    end
  end

  setup do
    AircloakCI.TestExec.clear_patterns()
    TestGithubAPI.subscribe()
    repo_data = new_repo_data()
    AircloakCI.Build.Branch.ensure_started(branch(repo_data, "master"), repo_data)
    {:ok, repo_data: repo_data}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp new_repo_data() do
    %{
      owner: repo().owner,
      name: repo().name,
      branches: [branch(name: "master")],
      pull_requests: []
    }
  end

  defp repo(), do:  %{owner: "test", name: "test"}

  defp branch(data) do
    Map.merge(%{sha: new_sha(), repo: repo()}, Map.new(data))
  end

  defp branch(repo_data, name), do:
    Enum.find(repo_data.branches, &(&1.name == name))

  def update_pr(repo_data, pr, updater) do
    update_in(
      repo_data,
      [Lens.key!(:pull_requests) |> Lens.all() |> Lens.filter(&(&1.number == pr.number))],
      updater
    )
  end

  defp job_outcomes(pr), do: AircloakCI.LocalProject.job_outcomes(pr_state(pr).project)

  defp pr_state(pr), do: pr |> pr_server() |> :sys.get_state()

  defp update_repo_data(pr, repo_data) do
    pr |> pr_server() |> send({:repo_data, repo_data})
    # a dummy sync request which ensures that the `:repo_data` message has been processed
    pr |> pr_server() |> :sys.get_state()
  end

  defp pr_server(pr) do
    [{pid, _}] = Registry.lookup(AircloakCI.Build.Registry, {:pull_request, pr.number})
    pid
  end
end
