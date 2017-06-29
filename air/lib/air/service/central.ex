defmodule Air.Service.Central do
  @moduledoc "Service functions related to central calls."
  require Logger
  import Supervisor.Spec
  import Ecto.Query, only: [from: 2]
  alias Air.Repo
  alias Air.Schemas.{CentralCall, ExportForAircloak}
  alias Air.Service.Central.RpcQueue

  @type rpc :: %{id: String.t, event: String.t, payload: map}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the supervisor specification for this service."
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    children =
    Enum.concat([
      [
        worker(Registry, [:unique, Air.Service.Central.Registry], id: Air.Service.Central.Registry),
        supervisor(Task.Supervisor, [[name: __MODULE__.TaskSupervisor]], [id: __MODULE__.TaskSupervisor]),
      ],
      case auto_export?() do
        false -> []
        true -> [worker(RpcQueue, [])]
      end,
      [supervisor(Air.CentralClient, [])]
    ])

    supervisor(Supervisor, [children, [strategy: :one_for_one, name: __MODULE__]], [id: __MODULE__])
  end

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

  @doc """
  Generates a new RPC entry which can be sent to the Central component.

  The `id` field can be of arbitrary type (including tuples, and refs) and size. Different RPCs need to have
  different ids, while the same RPC should always have the same id. The caller is responsible for choosing the
  `id` value which ensures these properties.
  """
  @spec new_rpc(any, String.t, map) :: rpc
  def new_rpc(id, event, payload), do:
    %{
      id: Base.encode64(:erlang.term_to_binary(id)),
      event: event,
      payload: payload
    }

  @doc "Asynchronously sends the query result to central."
  @spec report_query_result(map) :: :ok
  def report_query_result(result) do
    start_task(fn ->
      query = Repo.get!(Air.Schemas.Query, result.query_id) |> Repo.preload([:user, :data_source])

      user = query.user || %{name: "Unknown user", email: "Unknown email",}
      data_source = query.data_source || %{name: "Unknown data source", id: nil}
      row_count = result.row_count || 0

      Air.Service.Central.record_query(%{
        metrics: %{
          users_count: result[:users_count],
          row_count: row_count,
          execution_time: result[:execution_time],
        },
        features: result[:features],
        aux: %{
          user: %{
            name: user.name,
            email: user.email,
          },
          data_source: %{
            name: data_source.name,
            id: data_source.id,
          },
          started_at: query.inserted_at,
          finished_at: NaiveDateTime.utc_now(),
          has_error: not is_nil(result[:error]),
          error: filter_error(result[:error]),
        },
      })
    end)

    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_task(task_fun), do:
    Task.Supervisor.start_child(__MODULE__.TaskSupervisor, task_fun)

  defp enqueue_pending_call(event, payload) do
    if auto_export?() do
      RpcQueue.push(event, payload)
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
      rpcs: Enum.map(calls_to_export, &new_rpc(&1.id, &1.event, &1.payload)),
      air_name: Air.instance_name(),
      air_version: Aircloak.Version.for_app(:air) |> Aircloak.Version.to_string(),
      customer_token: Air.customer_token(),
    }
    |> Poison.encode!()
    |> :zlib.gzip()

  defp filter_error(nil), do: nil
  defp filter_error(error), do: Air.Service.Redacter.filter_query_error(error)
end
