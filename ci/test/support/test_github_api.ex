defmodule AircloakCI.TestGithubAPI do
  @moduledoc false

  def put_status_check_state(_owner, _repo, _sha, _context, _description, _state) do
    {:ok, %{category: :rest, remaining: 5000, expires_at: NaiveDateTime.utc_now()}}
  end

  def comment_on_issue(_owner, _repo, _issue_number, _body) do
    {:ok, %{category: :rest, remaining: 5000, expires_at: NaiveDateTime.utc_now()}}
  end
end
