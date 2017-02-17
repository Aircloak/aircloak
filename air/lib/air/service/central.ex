defmodule Air.Service.Central do
  @moduledoc "Service functions related to central calls."
  require Logger
  import Ecto.Query, only: [from: 2]
  alias Air.Repo
  alias Air.Schemas.{CentralCall, ExportForAircloak}


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

  @doc "Returns the time of oldest pending call."
  @spec oldest_pending_call_time() :: nil | NaiveDateTime.t
  def oldest_pending_call_time() do
    case Repo.one(from c in CentralCall, select: min(c.inserted_at)) do
      nil -> nil
      oldest_pending_call_time -> oldest_pending_call_time
    end
  end

  @doc "Exports all pending calls, returning export schema on success."
  @dialyzer {:no_opaque, export_pending_calls: 0} # Error in current Ecto: https://github.com/elixir-ecto/ecto/issues/1882
  @spec export_pending_calls() :: {:ok, ExportForAircloak.t} | {:error, :nothing_to_export | :database_error}
  def export_pending_calls() do
    with {:ok, calls_to_export} <- calls_to_export() do
      max_pending_call_id = calls_to_export |> Stream.map(&(&1.id)) |> Enum.max()

      Ecto.Multi.new()
      |> Ecto.Multi.insert(:stored_export, %ExportForAircloak{payload: payload(calls_to_export)})
      |> Ecto.Multi.delete_all(:delete_exported, from(c in CentralCall, where: c.id <= ^max_pending_call_id))
      |> Repo.transaction()
      |> case do
        {:ok, results} ->
          {:ok, results.stored_export}
        other ->
          Logger.error("Error storing export to the database #{inspect other}")
          {:error, :database_error}
      end
    end
  end

  @doc "Retrieves the existing export."
  @spec get_export!(non_neg_integer) :: ExportForAircloak.t
  def get_export!(export_id), do:
    Repo.get!(ExportForAircloak, export_id)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp calls_to_export() do
    case pending_calls() do
      [] -> {:error, :nothing_to_export}
      calls_to_export -> {:ok, calls_to_export}
    end
  end

  defp payload(calls_to_export), do:
    %{
      last_exported_id: Repo.one(from exported in ExportForAircloak, select: max(exported.id)),
      rpcs: Enum.map(calls_to_export, &CentralCall.export/1)
    }
    |> Poison.encode!()
    |> :zlib.gzip()
end
