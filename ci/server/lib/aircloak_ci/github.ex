defmodule AircloakCI.Github do
  @moduledoc "Wrappers for sending API requests to Github."

  @type pull_request :: %{
    repo: repo,
    number: pos_integer,
    title: String.t,
    source_branch: String.t,
    target_branch: String.t,
    approved?: true,
    status_checks: %{String.t => :expected | status_check_state}
  }

  @type repo :: %{owner: String.t, name: String.t}

  @type status_check_state :: :error | :failure | :pending | :success


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the list of pending pull requests."
  @spec pending_pull_requests(String.t, String.t) :: [pull_request]
  def pending_pull_requests(owner, repo), do:
    graphql_request(~s/
      query {
        repository(owner: "#{owner}", name: "#{repo}") {
          pullRequests(
            first: 100, states: OPEN,
            orderBy: {field: UPDATED_AT, direction: DESC}
          ) {
            nodes {
              number
              title
              headRefName
              baseRefName
              reviews(last: 1) {nodes {createdAt state}}
              commits(last: 1) {nodes {commit {oid status {contexts {context state}}}}}
            }
          }
        }
      }
    /)
    |> Map.fetch!("repository")
    |> Map.fetch!("pullRequests")
    |> Map.fetch!("nodes")
    |> Enum.map(&to_pr_data(%{owner: owner, name: repo}, &1))

  @doc "Sets the status check state for the given owner/repo/sha."
  @spec put_status_check_state!(String.t, String.t, String.t, String.t, status_check_state) :: :ok
  def put_status_check_state!(owner, repo, sha, context, state) do
    %{status_code: 201} =
      post_rest_request(
        "/repos/#{owner}/#{repo}/statuses/#{sha}",
        %{
          context: context,
          state: encode_status_check_state(state),
        }
      )

    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_pr_data(repo, raw_pr_data) do
    %{
      repo: repo,
      number: Map.fetch!(raw_pr_data, "number"),
      title: Map.fetch!(raw_pr_data, "title"),
      source_branch: Map.fetch!(raw_pr_data, "headRefName"),
      target_branch: Map.fetch!(raw_pr_data, "baseRefName"),
      approved?: match?(%{"reviews" => %{"nodes" => [%{"state" => "APPROVED"}]}}, raw_pr_data),
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
        |> Enum.map(fn(%{"context" => context, "state" => state}) -> {context, decode_status_check_state(state)} end)
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
    query
    |> post_graphql_request()
    |> Map.fetch!(:body)
    |> Poison.decode!()
    |> extract_data!()

  defp extract_data!(response) do
    if Map.has_key?(response, "errors") do
      raise "github error: #{inspect response}"
    else
      Map.fetch!(response, "data")
    end
  end

  defp post_graphql_request(query), do:
    post_rest_request("/graphql", %{query: query})

  defp post_rest_request(path, params), do:
    HTTPoison.post!(
      "https://api.github.com#{path}",
      Poison.encode!(params),
      [
        {"authorization", "bearer #{System.get_env("AIRCLOAK_CI_AUTH")}"},
        {"Content-Type", "application/json"}
      ],
      [timeout: :timer.seconds(30), recv_timeout: :timer.seconds(30)]
    )
end
