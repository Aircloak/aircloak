defmodule AircloakCI.Github do
  @moduledoc """
  Rate-limited wrapper around Github API.

  This GenServer is used to synchronize all API calls to Github. The server ensures that we're issuing no more than one
  request per second.
  """

  use GenServer, start: {__MODULE__, :start_link, []}
  alias AircloakCI.Github
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the repository data, such as branches and open pull requests."
  @spec repo_data(String.t, String.t) :: Github.API.repo_data
  def repo_data(owner, repo), do:
    sync_request!(:repo_data, [owner, repo], type: :read)

  @doc "Returns the data for the given pull request."
  @spec pull_request(String.t, String.t, integer) :: Github.API.pull_request
  def pull_request(owner, repo, number), do:
    sync_request!(:pull_request, [owner, repo, number], type: :read)

  @doc "Sets the status check state for the given owner/repo/sha."
  @spec put_status_check_state(String.t, String.t, String.t, String.t, String.t, Github.API.status_check_state) :: :ok
  def put_status_check_state(owner, repo, sha, context, description, state), do:
    async_request(:put_status_check_state, [owner, repo, sha, context, description, state], type: :write)

  @doc "Posts a comment to the given issue or pull request."
  @spec comment_on_issue(String.t, String.t, pos_integer, String.t) :: :ok
  def comment_on_issue(owner, repo, issue_number, body), do:
    async_request(:comment_on_issue, [owner, repo, issue_number, body], type: :write)

  @doc "Posts a comment to the given issue or pull request."
  @spec comment_on_commit(String.t, String.t, number, String.t) :: :ok
  def comment_on_commit(owner, repo, sha, body), do:
    async_request(:comment_on_commit, [owner, repo, sha, body], type: :write)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    Process.flag(:trap_exit, true)
    enqueue_clear()
    :timer.send_interval(:timer.minutes(10), :log_rate_limits)
    {:ok, %{clear?: false, current_request: nil, request_job: nil, queue: :queue.new(), rate_limits: %{}}}
  end

  @impl GenServer
  def handle_call({:request, fun, args, opts}, from, state), do:
    {:noreply, handle_request(state, %{fun: fun, args: args, from: from}, opts)}

  @impl GenServer
  def handle_cast({:request, fun, args, opts}, state), do:
    {:noreply, handle_request(state, %{fun: fun, args: args, from: nil}, opts)}

  @impl GenServer
  def handle_info(:clear, state), do:
    {:noreply, maybe_start_next_request(%{state | clear?: true})}
  def handle_info({ref, result}, %{request_job: %Task{ref: ref}} = state) do
    {response, rate_limit} = result
    {:noreply, state |> update_rate_limit(rate_limit) |> handle_current_request_finished({:ok, response})}
  end
  def handle_info({:DOWN, ref, :process, _, _reason}, %{request_job: %Task{ref: ref}} = state), do:
    {:noreply, handle_current_request_finished(state, :error)}
  def handle_info({:DOWN, _, :process, _, :normal}, state), do:
    # previous job exited normally -> we already removed it from the state, so nothing to do here
    {:noreply, state}
  def handle_info({:EXIT, _, _}, state), do:
    # ignoring task exits, since we handled DOWN and result messages
    {:noreply, state}
  def handle_info(:log_rate_limits, state) do
    # remove outdated rate limits
    state =
      update_in(
        state.rate_limits,
        &Enum.reject(&1, fn({_category, rate_limit}) -> expires_in(rate_limit) <= 0 end)
      )

    Enum.each(state.rate_limits,
      fn({category, rate_limit}) ->
        Logger.info("#{category} #{rate_limit.remaining} requests remaining, expires in #{expires_in(rate_limit)} sec")
      end
    )
    {:noreply, state}
  end
  def handle_info(other, state), do:
    super(other, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sync_request!(fun, args, opts) do
    {:ok, result} = GenServer.call(__MODULE__, {:request, fun, args, opts}, :timer.seconds(30))
    result
  end

  defp async_request(fun, args, opts), do:
    GenServer.cast(__MODULE__, {:request, fun, args, opts})

  defp enqueue_clear(), do:
    Process.send_after(self(), :clear, :timer.seconds(1))

  defp handle_request(state, request, opts) do
    if Keyword.fetch!(opts, :type) == :write and Application.get_env(:aircloak_ci, :simulate_github_writes) do
      IO.puts "simulated github write #{request.fun}(#{request.args |> Enum.map(&inspect/1) |> Enum.join(", ")})"
      state
    else
      append_request(state, request)
    end
  end

  defp append_request(state, request), do:
    state.queue
    |> update_in(&:queue.in(request, &1))
    |> maybe_start_next_request()

  defp maybe_start_next_request(%{clear?: true, request_job: nil} = state) do
    case :queue.out(state.queue) do
      {{:value, request}, queue} ->
        enqueue_clear()
        %{state | clear?: false, queue: queue, current_request: request, request_job: start_request_job(request)}

      {:empty, _} ->
        state
    end
  end
  defp maybe_start_next_request(state), do:
    state

  defp start_request_job(request), do:
    Task.async(fn -> apply(Github.API, request.fun, request.args) end)

  defp handle_current_request_finished(state, response) do
    if not is_nil(state.current_request.from), do: GenServer.reply(state.current_request.from, response)
    maybe_start_next_request(%{state | current_request: nil, request_job: nil})
  end

  defp update_rate_limit(state, nil), do: state
  defp update_rate_limit(state, rate_limit), do: put_in(state.rate_limits[rate_limit.category], rate_limit)

  defp expires_in(rate_limit), do:
    DateTime.diff(rate_limit.expires_at, DateTime.utc_now(), :second)


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do:
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
