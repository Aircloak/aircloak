defmodule Air.Service.Central do
  @moduledoc "Service functions related to central calls."
  require Logger
  alias Air.Repo
  alias Air.Schemas.CentralCall


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Persists a pending central call."
  @spec store_pending_call(String.t, map) :: {:ok, CentralCall.t} | :error
  def store_pending_call(event, payload) do
    changeset = CentralCall.changeset(%CentralCall{}, %{event: event, payload: payload})
    case Repo.insert(changeset) do
      {:error, changeset} ->
        Logger.error("Unable to persist RPC call to central to guarantee delivery. Aborting RPC. "
          <> "Failure: #{inspect changeset}")
        :error
      {:ok, _} = result -> result
    end
  end

  @doc "Removes a pending central call."
  @spec remove_pending_call!(CentralCall.t) :: CentralCall.t
  def remove_pending_call!(central_call), do:
    Repo.delete!(central_call)

  @doc "Returns all pending central calls."
  @spec pending_calls() :: [CentralCall.t]
  def pending_calls(), do:
    Repo.all(CentralCall)
end
