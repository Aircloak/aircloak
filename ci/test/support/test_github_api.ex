defmodule AircloakCI.TestGithubAPI do
  @moduledoc false

  def subscribe() do
    Process.register(self(), __MODULE__.Subscriber)
    :ok
  end

  def put_status_check_state(owner, repo, sha, context, description, state) do
    send(
      __MODULE__.Subscriber,
      {:posted_status, %{owner: owner, repo: repo, sha: sha, context: context, description: description, state: state}}
    )

    {:ok, %{category: :rest, remaining: 5000, expires_at: NaiveDateTime.utc_now()}}
  end

  def comment_on_issue(owner, repo, issue_number, body) do
    send(
      __MODULE__.Subscriber,
      {:commented_on_issue, %{owner: owner, repo: repo, issue_number: issue_number, body: body}}
    )

    {:ok, %{category: :rest, remaining: 5000, expires_at: NaiveDateTime.utc_now()}}
  end
end
