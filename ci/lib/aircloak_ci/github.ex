defmodule AircloakCI.Github do
  @moduledoc """
  Rate-limited wrapper around Github API.

  This module is used to synchronize all API calls to Github. It ensures that we're issuing no more than one request
  per second.
  """

  alias AircloakCI.{Github, Queue}
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the repository data, such as branches and open pull requests."
  @spec repo_data(String.t(), String.t()) :: Github.API.repo_data()
  def repo_data(owner, repo), do: sync_request!(:repo_data, [owner, repo], type: :read)

  @doc "Sets the status check state for the given owner/repo/sha."
  @spec put_status_check_state(
          String.t(),
          String.t(),
          String.t(),
          String.t(),
          String.t(),
          Github.API.status_check_state()
        ) :: :ok
  def put_status_check_state(owner, repo, sha, context, description, state) do
    retries = if state == :success, do: 60, else: 0

    async_request(
      :put_status_check_state,
      [owner, repo, sha, context, description, state],
      type: :write,
      retries: retries
    )
  end

  @doc "Posts a comment to the given issue or pull request."
  @spec comment_on_issue(String.t(), String.t(), pos_integer, String.t()) :: :ok
  def comment_on_issue(owner, repo, issue_number, body),
    do:
      async_request(
        :comment_on_issue,
        [owner, repo, issue_number, body],
        type: :write,
        retries: 10
      )

  @doc "Posts a comment to the given issue or pull request."
  @spec comment_on_commit(String.t(), String.t(), number, String.t()) :: :ok
  def comment_on_commit(owner, repo, sha, body),
    do: async_request(:comment_on_commit, [owner, repo, sha, body], type: :write)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp async_request(fun, args, opts) do
    Task.Supervisor.start_child(__MODULE__, fn -> sync_request!(fun, args, opts) end)
    :ok
  end

  defp sync_request!(fun, args, opts) do
    {response, rate_limit} =
      invoke_github_api_with_retries(fun, args, opts, Keyword.get(opts, :retries, 0))

    :ets.insert(__MODULE__, {rate_limit.category, rate_limit})
    response
  end

  defp invoke_github_api_with_retries(fun, args, opts, 0), do: invoke_github_api(fun, args, opts)

  defp invoke_github_api_with_retries(fun, args, opts, retries) do
    invoke_github_api(fun, args, opts)
  catch
    type, error ->
      formatted_invocation = "#{inspect(fun)}(#{args |> Enum.map(&inspect/1) |> Enum.join(", ")})"
      formatted_error = Exception.format(type, error, System.stacktrace())
      Logger.error("Github API error in #{formatted_invocation}: #{formatted_error}")
      :timer.sleep(:timer.seconds(30))
      invoke_github_api_with_retries(fun, args, opts, retries - 1)
  end

  defp invoke_github_api(fun, args, opts) do
    if Keyword.fetch!(opts, :type) == :write and
         not Application.get_env(:aircloak_ci, :write_to_github) do
      IO.puts(
        "\nsimulated github write #{fun}(#{args |> Enum.map(&inspect/1) |> Enum.join(", ")})\n"
      )

      {:ok, %{category: :rest, remaining: 5000, expires_at: DateTime.utc_now()}}
    else
      api_mod = Application.get_env(:aircloak_ci, :github_api, Github.API)
      Queue.exec(:github_api, fn -> apply(api_mod, fun, args) end)
    end
  end

  @doc false
  def log_rate_limits() do
    for [rate_limit] <- :ets.match(__MODULE__, {:_, :"$1"}), expires_in(rate_limit) >= 0 do
      Logger.info([
        "#{rate_limit.category} #{rate_limit.remaining} requests remaining, ",
        "expires in #{expires_in(rate_limit)} sec"
      ])
    end
  end

  defp expires_in(rate_limit),
    do: DateTime.diff(rate_limit.expires_at, DateTime.utc_now(), :second)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_),
    do: Supervisor.child_spec(Task.Supervisor, start: {__MODULE__, :start_link, []})

  @doc false
  def start_link() do
    if :ets.info(__MODULE__) == :undefined do
      :ets.new(__MODULE__, [
        :named_table,
        :public,
        read_concurrency: true,
        write_concurrency: true
      ])

      :timer.apply_interval(:timer.minutes(10), __MODULE__, :log_rate_limits, [])
    end

    Task.Supervisor.start_link(name: __MODULE__)
  end
end
