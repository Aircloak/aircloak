defmodule AircloakCI.Builder.Server do
  @moduledoc "Process which starts and manages CI builds."

  use GenServer, start: {__MODULE__, :start_link, []}
  alias AircloakCI.Builder


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    Process.flag(:trap_exit, true)
    enqueue_pr_check()
    {:ok, Builder.new()}
  end

  @impl GenServer
  def handle_info(:check_for_prs, builder) do
    pending_prs = pending_prs()
    enqueue_pr_check()
    {:noreply, Enum.reduce(pending_prs, builder, &Builder.process_pr(&2, &1))}
  end
  def handle_info(message, builder) do
    case Builder.handle_message(builder, message) do
      {:ok, state} -> {:noreply, state}
      :error -> super(message, builder)
    end
  end

  defp enqueue_pr_check(), do:
    Process.send_after(self(), :check_for_prs, :timer.seconds(5))

  defp pending_prs(), do:
    AircloakCI.Github.RateLimiter.pending_pull_requests("aircloak", "aircloak")


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do:
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
