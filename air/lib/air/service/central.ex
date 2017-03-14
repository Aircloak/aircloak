defmodule Air.Service.Central do
  @moduledoc "Service functions related to central calls."
  require Logger
  import Ecto.Query, only: [from: 2]
  alias Air.Repo
  alias Air.Schemas.{CentralCall, ExportForAircloak}
  alias Air.Service.Central.CallsQueue


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if auto export mode is used to communicate with central."
  @spec auto_export?() :: boolean
  def auto_export?(), do:
    Aircloak.DeployConfig.override_app_env!(:air, :auto_aircloak_export)

  @doc "Returns true if manual export mode is used to communicate with central."
  @spec manual_export?() :: boolean
  if Mix.env == :dev do
    # In dev mode, we're having both, auto and manual export enabled for easier development. Note that we're
    # not returning `true` explicitly to suppress some dialyzer warnings.
    def manual_export?(), do: auto_export?()
  else
    def manual_export?(), do: not auto_export?()
  end

  @doc "Records a completed query in the central - useful for billing and stats"
  @spec record_query(Map.t) :: :ok
  def record_query(payload), do:
    enqueue_pending_call("query_execution", payload)

  @doc "Records a connection of a cloak in the central."
  @spec record_cloak_online(String.t, [String.t], String.t) :: :ok
  def record_cloak_online(cloak_name, data_source_names, version), do:
    enqueue_pending_call("cloak_online", %{name: cloak_name, data_source_names: data_source_names, version: version})

  @doc "Records a disconnection of a cloak in the central."
  @spec record_cloak_offline(String.t) :: :ok
  def record_cloak_offline(cloak_name), do:
    enqueue_pending_call("cloak_offline", %{name: cloak_name})

  @doc "Sends usage info to central."
  @spec send_usage_info(Map.t) :: :ok
  def send_usage_info(usage_info), do:
    enqueue_pending_call("usage_info", usage_info)

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
  # Error in current Ecto: https://github.com/elixir-ecto/ecto/issues/1882
  @dialyzer {:no_opaque, export_pending_calls: 0}
  @spec export_pending_calls() :: {:ok, ExportForAircloak.t} | {:error, :nothing_to_export | :database_error}
  def export_pending_calls() do
    with {:ok, calls_to_export} <- calls_to_export() do
      max_pending_call_id = Enum.max_by(calls_to_export, &(&1.id)).id

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

  @doc "Retrieves exports of the given page."
  @spec exports(pos_integer, pos_integer) :: Scrivener.Page.t
  def exports(page, page_size), do:
    Repo.paginate(
      from(e in ExportForAircloak, select: e, order_by: [desc: e.id]),
      page: page, page_size: page_size
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp enqueue_pending_call(event, payload) do
    if auto_export?() do
      CallsQueue.perform_rpc(CentralCall.new(event, payload))
    else
      {:ok, _} = store_pending_call(event, payload)
      :ok
    end
  end

  defp calls_to_export() do
    case pending_calls() do
      [] -> {:error, :nothing_to_export}
      calls_to_export -> {:ok, calls_to_export}
    end
  end

  defp payload(calls_to_export), do:
    %{
      last_exported_id: Repo.one(from exported in ExportForAircloak, select: max(exported.id)),
      rpcs: Enum.map(calls_to_export, &CentralCall.export/1),
      air_name: Air.instance_name(),
      air_version: Aircloak.Version.for_app(:air) |> Aircloak.Version.to_string(),
      customer_token: Air.customer_token(),
    }
    |> Poison.encode!()
    |> :zlib.gzip()
end
