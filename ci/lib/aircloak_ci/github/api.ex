defmodule AircloakCI.Github.API do
  @moduledoc """
  Wrappers for sending API requests to Github.

  ## Important note

  Since Github API is rate-limited, you shouldn't use this module directly from the rest of the code.
  Instead, you should use the rate-limiting wrapper `AircloakCI.Github`.
  """

  @type repo_data :: %{
    owner: String.t,
    name: String.t,
    branches: [branch],
    pull_requests: [pull_request],
  }

  @type branch :: %{name: String.t, sha: String.t, repo: repo}

  @type pull_request :: %{
    repo: repo,
    number: pos_integer,
    title: String.t,
    source_branch: String.t,
    target_branch: String.t,
    sha: String.t,
    mergeable?: boolean,
    merge_sha: String.t,
    approved?: boolean,
    status_checks: statuses
  }

  @type repo :: %{owner: String.t, name: String.t}

  @type statuses :: %{String.t => %{status: :expected | status_check_state, description: String.t}}

  @type status_check_state :: :error | :failure | :pending | :success

  @type rate_limit :: %{category: :graphql | :rest, remaining: non_neg_integer, expires_at: DateTime.t} | nil


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the repository data, such as branches and open pull requests."
  @spec repo_data(String.t, String.t) :: {repo_data, rate_limit}
  def repo_data(owner, repo_name) do
    result = graphql_request("query {#{repo_query(owner, repo_name, "#{branches_query()} #{prs_query()}")}}")
    repository = Map.fetch!(result.response, "repository")
    repo = %{owner: owner, name: repo_name}
    repo_data =
      %{
        owner: owner,
        name: repo_name,
        branches:
          repository
          |> Map.fetch!("refs")
          |> Map.fetch!("nodes")
          |> Enum.map(fn(%{"name" => name, "target" => %{"oid" => sha}}) -> %{name: name, sha: sha, repo: repo} end),
        pull_requests:
          repository
          |> Map.fetch!("pullRequests")
          |> Map.fetch!("nodes")
          |> Enum.map(&to_pr_data(&1, repo))
      }

    {repo_data, result.rate_limit}
  end

  @doc "Sets the status check state for the given owner/repo/sha."
  @spec put_status_check_state(String.t, String.t, String.t, String.t, String.t, status_check_state) ::
    {:ok, rate_limit}
  def put_status_check_state(owner, repo, sha, context, description, state) do
    %{response: %{status_code: 201}, rate_limit: rate_limit} =
      post_rest_request(
        "/repos/#{owner}/#{repo}/statuses/#{sha}",
        %{
          context: context,
          description: description,
          state: encode_status_check_state(state),
        }
      )

    {:ok, rate_limit}
  end

  @doc "Posts a comment to the given issue or pull request."
  @spec comment_on_issue(String.t, String.t, pos_integer, String.t) :: {:ok, rate_limit}
  def comment_on_issue(owner, repo, issue_number, body), do:
    comment_on_commit(owner, repo, "issues", issue_number, body)

  @doc "Posts a comment to the given commit."
  @spec comment_on_commit(String.t, String.t, String.t, String.t) :: {:ok, rate_limit}
  def comment_on_commit(owner, repo, sha, body), do:
    comment_on_commit(owner, repo, "commits", sha, body)


  # -------------------------------------------------------------------
  # GraphQL queries
  # -------------------------------------------------------------------

  defp repo_query(owner, repo, inner_query), do:
    ~s/repository(owner: "#{owner}", name: "#{repo}") {#{inner_query}}/

  defp branches_query(), do:
    ~s[refs(refPrefix: "refs/heads/", first: 100) {nodes {name target {oid}}}]

  defp prs_query(), do:
    ~s/
      pullRequests(first: 100, states: OPEN, orderBy: {field: UPDATED_AT, direction: DESC}) {
        nodes{#{pr_fields_query()}}
      }
    /

  defp pr_fields_query(), do:
    ~s/
      number
      title
      mergeable
      potentialMergeCommit {oid}
      headRefName
      baseRefName
      reviews(states: [APPROVED, DISMISSED, CHANGES_REQUESTED], last: 1) {nodes {state}}
      commits(last: 1) {nodes {commit {oid status {contexts {context state description}}}}}
    /


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_pr_data(raw_pr_data, repo) do
    %{
      repo: repo,
      number: Map.fetch!(raw_pr_data, "number"),
      title: Map.fetch!(raw_pr_data, "title"),
      source_branch: Map.fetch!(raw_pr_data, "headRefName"),
      target_branch: Map.fetch!(raw_pr_data, "baseRefName"),
      approved?: match?(%{"reviews" => %{"nodes" => [%{"state" => "APPROVED"}]}}, raw_pr_data),
      mergeable?: Map.fetch!(raw_pr_data, "mergeable") == "MERGEABLE",
      merge_sha: raw_pr_data["potentialMergeCommit"]["oid"],
      sha:
        raw_pr_data
        |> Map.fetch!("commits")
        |> Map.fetch!("nodes")
        |> hd()
        |> Map.fetch!("commit")
        |> Map.fetch!("oid"),
      status_checks:
        raw_pr_data
        |> Map.fetch!("commits")
        |> Map.fetch!("nodes")
        |> Enum.flat_map(&(Map.fetch!(&1, "commit")["status"]["contexts"] || []))
        |> Enum.map(fn(%{"context" => context, "state" => state, "description" => description}) ->
            {context, %{status: decode_status_check_state(state), description: description}}
          end)
        |> Enum.into(%{})
    }
  end

  defp decode_status_check_state("EXPECTED"), do: :expected
  defp decode_status_check_state("ERROR"), do: :error
  defp decode_status_check_state("FAILURE"), do: :failure
  defp decode_status_check_state("PENDING"), do: :pending
  defp decode_status_check_state("SUCCESS"), do: :success

  defp encode_status_check_state(:error), do: "error"
  defp encode_status_check_state(:failure), do: "failure"
  defp encode_status_check_state(:pending), do: "pending"
  defp encode_status_check_state(:success), do: "success"

  defp graphql_request(query), do:
    update_in(post_graphql_request(query).response, &(
      &1
      |> Map.fetch!(:body)
      |> Poison.decode!()
      |> extract_data!()
    ))

  defp extract_data!(response) do
    if Map.has_key?(response, "errors") do
      raise "github error: #{inspect response}"
    else
      Map.fetch!(response, "data")
    end
  end

  defp post_graphql_request(query), do:
    post_rest_request("/graphql", %{query: query})

  defp post_rest_request(path, params) do
    response =
      HTTPoison.post!(
        "https://api.github.com#{path}",
        Poison.encode!(params),
        [
          {"authorization", "bearer #{AircloakCI.github_token!()}"},
          {"Content-Type", "application/json"}
        ],
        [timeout: :timer.seconds(30), recv_timeout: :timer.seconds(30)]
      )

    %{response: response, rate_limit: rate_limit(response, path)}
  end

  defp rate_limit(response, path) do
    with \
      {_, remaining} <- Enum.find(response.headers, &match?({"X-RateLimit-Remaining", _value}, &1)),
      {remaining_int, ""} <- Integer.parse(remaining),
      {_, reset} <- Enum.find(response.headers, &match?({"X-RateLimit-Reset", _value}, &1)),
      {reset_int, ""} <- Integer.parse(reset),
      {:ok, reset_time} <- DateTime.from_unix(reset_int)
    do
      %{
        category: (if path == "/graphql", do: :graphql, else: :rest),
        remaining: remaining_int,
        expires_at: reset_time
      }
    else
      _ -> nil
    end
  end

  defp comment_on_commit(owner, repo, type, id, body) do
    %{response: %{status_code: 201}, rate_limit: rate_limit} =
      post_rest_request(
        "/repos/#{owner}/#{repo}/#{type}/#{id}/comments",
        %{body: body}
      )

    {:ok, rate_limit}
  end
end
