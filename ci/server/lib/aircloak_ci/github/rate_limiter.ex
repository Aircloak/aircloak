defmodule AircloakCI.Github.RateLimiter do
  @moduledoc """
  Rate limiter for Github API operations.

  This GenServer is used to synchronize all API calls to Github. The server ensures that we're issuing no more than one
  request per second.
  """

  use GenServer, start: {__MODULE__, :start_link, []}
  alias AircloakCI.Github


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the list of pending pull requests."
  @spec pending_pull_requests(String.t, String.t) :: [Github.pull_request]
  def pending_pull_requests(owner, repo), do:
    sync_request!(:pending_pull_requests, [owner, repo])

  @doc "Returns the data for the given pull request."
  @spec pull_request(String.t, String.t, integer) :: Github.pull_request
  def pull_request(owner, repo, number), do:
    sync_request!(:pull_request, [owner, repo, number])

  @doc "Sets the status check state for the given owner/repo/sha."
  @spec put_status_check_state!(String.t, String.t, String.t, String.t, Github.status_check_state) :: :ok
  def put_status_check_state!(owner, repo, sha, context, state), do:
    sync_request!(:put_status_check_state!, [owner, repo, sha, context, state])

  @doc "Posts a comment to the given issue or pull request."
  @spec post_comment(String.t, String.t, number, String.t) :: :ok
  def post_comment(owner, repo, issue_number, body), do:
    sync_request!(:post_comment, [owner, repo, issue_number, body])


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    Process.flag(:trap_exit, true)
    enqueue_clear()
    {:ok, %{clear?: false, current_request: nil, request_job: nil, queue: :queue.new()}}
  end

  @impl GenServer
  def handle_call({:request, fun, args}, from, state), do:
    {:noreply, append_request(state, %{fun: fun, args: args, from: from})}

  @impl GenServer
  def handle_info(:clear, state), do:
    {:noreply, maybe_start_next_request(%{state | clear?: true})}
  def handle_info({ref, result}, %{request_job: %Task{ref: ref}} = state) do
    GenServer.reply(state.current_request.from, {:ok, result})
    {:noreply, maybe_start_next_request(%{state | current_request: nil, request_job: nil})}
  end
  def handle_info({:DOWN, ref, :process, _, _reason}, %{request_job: %Task{ref: ref}} = state) do
    GenServer.reply(state.current_request.from, :error)
    {:noreply, maybe_start_next_request(%{state | current_request: nil, request_job: nil})}
  end
  def handle_info({:DOWN, _, :process, _, :normal}, state), do:
    # previous job exited normally -> we already removed it from the state, so nothing to do here
    {:noreply, state}
  def handle_info({:EXIT, _, _}, state), do:
    # ignoring task exits, since we handled DOWN and result messages
    {:noreply, state}
  def handle_info(other, state), do:
    super(other, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sync_request!(fun, args) do
    {:ok, result} = GenServer.call(__MODULE__, {:request, fun, args}, :timer.seconds(30))
    result
  end

  defp enqueue_clear(), do:
    Process.send_after(self(), :clear, :timer.seconds(1))

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
    Task.async(fn -> apply(Github, request.fun, request.args) end)


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do:
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
