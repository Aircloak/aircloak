defmodule AircloakCI.Builder.Server do
  @moduledoc "Process which starts and manages CI builds."

  use GenServer, start: {__MODULE__, :start_link, []}
  alias AircloakCI.{Builder, Github}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Force starts the build of the given pull request."
  @spec force_build(pos_integer) :: :ok | {:error, String.t}
  def force_build(pull_request_number) do
    pr = Github.pull_request("aircloak", "aircloak", pull_request_number)
    GenServer.call(__MODULE__, {:force_build, pr}, :timer.minutes(1))
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    Process.flag(:trap_exit, true)
    AircloakCI.RepoDataProvider.subscribe()
    {:ok, Builder.new()}
  end

  @impl GenServer
  def handle_call({:force_build, pr}, _from, builder) do
    {result, builder} = Builder.force_build(builder, pr)
    {:reply, result, builder}
  end

  @impl GenServer
  def handle_info({:repo_data, repo_data}, builder), do:
    {:noreply, Builder.process_prs(builder, repo_data)}
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
