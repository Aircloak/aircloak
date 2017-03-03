defmodule Air.Service.Central.Worker do
  @moduledoc "Serializes requests to central."

  use GenServer

  alias Air.{Repo, Schemas.CentralCall}

  import Ecto.Query
  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc false
  @spec start_link() :: GenServer.on_start
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc "Schedules the given central call to be performed asynchronously."
  @spec perform_rpc(CentralCall.t) :: :ok
  def perform_rpc(central_call), do: GenServer.cast(__MODULE__, {:perform_rpc, central_call})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_), do: {:ok, nil}

  @doc false
  def handle_cast({:perform_rpc, central_call}, state) do
    case Air.CentralClient.Socket.rpc(CentralCall.export(central_call)) do
      {:ok, _} ->
        remove_pending_call!(central_call)
      {:error, reason} ->
        Logger.error("RPC '#{central_call.event}' to central failed: #{inspect reason}. Will retry later.")
    end

    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp remove_pending_call!(central_call) do
    Repo.delete_all(from c in CentralCall, where: c.id == ^central_call.id)
    :ok
  end
end
