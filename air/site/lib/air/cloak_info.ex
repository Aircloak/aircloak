defmodule Air.CloakInfo do
  @moduledoc """
  Storing and retrieving information about connected cloaks.

  This module powers a process which maintains information about the connected cloak
  in etcd. The process stores and periodically renews the cloak data. If the
  process is terminated, cloak data is immediately deleted. If the whole node is
  abruptly taken down, data will expire after some timeout. Finally, if this node
  ends up in a minority, renewal will fail, and the process will crash, causing
  the channel to be closed.
  """

  require Logger
  alias Air.Utils.Process, as: ProcessUtils

  defstruct [:id, :name, :organisation, :data_sources, :created_at]

  @type t :: %__MODULE__{
    id: cloak_id,
    name: String.t,
    organisation: String.t,
    data_sources: [data_source],
    created_at: integer # connect timestamp, with second-level accuracy
  }
  @type cloak_id :: String.t
  @type data_source :: %{id: String.t, tables: [table]}
  @type table :: %{id: String.t, columns: [column]}
  @type column :: %{name: String.t, type: String.t}

  @registration_root_key "/settings/air/cloaks"


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the process."
  @spec start_link(%{String.t => any}) :: GenServer.server
  def start_link(raw_cloak_info) do
    cloak_info = parse_cloak_info(raw_cloak_info)
    store_cloak_info(cloak_info)

    Air.ServiceRegistration.start_link(
      etcd_path(cloak_info.id),
      encode_cloak_data(%{pid: self()}),
      crash_on_error: true
    )
  end

  @doc """
  Returns all cloaks belonging to the given organisation.

  If the organisation is the admin org, all cloaks of all organisations are returned.
  """
  @spec all(Air.Organisation.t) :: [t]
  def all(organisation) do
    all_connected = connected_cloaks()
    if Air.Organisation.admins?(organisation) do
      all_connected
    else
      Enum.filter(all_connected, &(&1.organisation == organisation.name))
    end
  end

  @doc "Returns the information for the given cloak."
  @spec get(cloak_id) :: t | nil
  def get(cloak_id) do
    case cloak_data(cloak_id) do
      nil -> nil
      cloak_data -> cloak_data.cloak_info
    end
  end

  @doc "Returns the pid of the main channel of the given cloak."
  @spec main_channel_pid(cloak_id) :: pid | nil
  def main_channel_pid(cloak_id) do
    case cloak_data(cloak_id) do
      nil -> nil
      cloak_data -> cloak_data.pid
    end
  end

  @doc """
  Generates a deterministic cloak_id from organisation and name.

  The internal format of the generated id is chosen for simpler debugging and
  analysis. By looking at the ID, it should be easy to find out the related
  cloak. However, you shouldn't rely on this shape in the client code, since it
  might change in the future.
  """
  @spec cloak_id(String.t, String.t) :: String.t
  def cloak_id(organisation, name) do
    inspect({organisation, name})
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp connected_cloaks() do
    for {etcd_path, _} <- :air_etcd.ls(@registration_root_key),
        cloak_data = fetch_cloak_data("#{etcd_path}/main"),
        cloak_data != nil
    do
      cloak_data.cloak_info
    end
  end

  defp cloak_data(cloak_id) do
    fetch_cloak_data(etcd_path(cloak_id))
  end

  defp fetch_cloak_data(etcd_path) do
    case :air_etcd.fetch(etcd_path) do
      {:ok, encoded_cloak_data} when is_binary(encoded_cloak_data) ->
        cloak_data = decode_cloak_data(encoded_cloak_data)
        if ProcessUtils.alive?(cloak_data.pid) do
          # keeps false positives out, i.e. processes which have terminated, but the entry still lingers on
          Map.put(cloak_data, :cloak_info, fetch_cloak_info(cloak_data.pid))
        else
          nil
        end
      _ ->
        nil
    end
  end

  defp store_cloak_info(cloak_info) do
    true = :gproc.reg(gproc_name(self()), cloak_info)
  end

  @doc false
  def fetch_cloak_info(main_channel_pid) do
    if node(main_channel_pid) == node() do
      :gproc.lookup_value(gproc_name(main_channel_pid))
    else
      case :rpc.call(node(main_channel_pid), __MODULE__, :fetch_cloak_info, [main_channel_pid]) do
        {:badrpc, reason} ->
          Logger.error("Error retrieving cloak info #{inspect reason}")
          raise "can't fetch cloak info"

        cloak_info -> cloak_info
      end
    end
  end

  defp gproc_name(main_channel_pid) do
    {:n, :l, {__MODULE__, main_channel_pid}}
  end

  defp etcd_path(cloak_id) do
    # base32 is used because the supported character set in the etcd key is limited
    "#{@registration_root_key}/#{Base.encode32(cloak_id)}/main"
  end

  defp encode_cloak_data(cloak_data) do
    cloak_data
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  defp decode_cloak_data(encoded_cloak_data) do
    encoded_cloak_data
    |> Base.decode64!()
    |> :erlang.binary_to_term()
  end

  defp parse_cloak_info(cloak_info) do
    %__MODULE__{
      id: cloak_id(cloak_info.organisation, cloak_info.name),
      name: cloak_info.name,
      organisation: cloak_info.organisation,
      data_sources: parse_data_sources(cloak_info.data_sources),
      created_at: Timex.DateTime.now() |> Timex.to_erlang_datetime() |> Ecto.DateTime.from_erl(),
    }
  end

  defp parse_data_sources(data_sources) do
    for data_source <- data_sources do
      %{
        id: Map.fetch!(data_source, "id"),
        tables: Map.fetch!(data_source, "tables") |> parse_tables()
      }
    end
  end

  defp parse_tables(tables) do
    for table <- tables do
      %{
        id: Map.fetch!(table, "id"),
        columns: Map.fetch!(table, "columns") |> parse_columns()
      }
    end
  end

  defp parse_columns(columns) do
    for column <- columns do
      %{
        name: Map.fetch!(column, "name"),
        type: Map.fetch!(column, "type")
      }
    end
  end
end
