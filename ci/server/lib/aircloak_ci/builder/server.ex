defmodule AircloakCI.Builder.Server do
  @moduledoc "Process which starts and manages CI builds."

  use GenServer, start: {__MODULE__, :start_link, []}
  alias AircloakCI.Builder


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Handles pending pull requests."
  @spec handle_pending_prs([AircloakCI.Github.pull_request]) :: :ok
  def handle_pending_prs(pending_prs), do:
    GenServer.call(__MODULE__, {:handle_pending_prs, pending_prs})

  @doc "Force starts the build of the given pull request."
  @spec force_build(AircloakCI.Github.pull_request) :: :ok | {:error, String.t}
  def force_build(pr), do:
    GenServer.call(__MODULE__, {:force_build, pr})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    Process.flag(:trap_exit, true)
    {:ok, Builder.new()}
  end

  @impl GenServer
  def handle_call({:handle_pending_prs, pending_prs}, _from, builder), do:
    {:reply, :ok, Enum.reduce(pending_prs, builder, &Builder.process_pr(&2, &1))}
  def handle_call({:force_build, pr}, _from, builder) do
    case Builder.force_build(builder, pr) do
      {:ok, new_builder} -> {:reply, :ok, new_builder}
      error -> {:reply, error, builder}
    end
  end

  @impl GenServer
  def handle_info(message, builder) do
    case Builder.handle_message(builder, message) do
      {:ok, state} -> {:noreply, state}
      :error -> super(message, builder)
    end
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do:
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
