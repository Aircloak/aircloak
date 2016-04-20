defmodule Air.CloakInfo do
  @moduledoc """
  Storing and retrieving information about connected cloaks.

  This module powers a process which maintains information about the connected cloak
  in etcd. The process will stores and periodically renews the cloak data. If the
  process is terminated, cloak data is immediately deleted. If the whole node is
  abruptly taken down, data will expire after some timeout. Finally, if this node
  ends up in a minority, renewal will fail, and the process will crash, causing
  the channel to be closed.
  """
  defstruct [:name, :organisation, :data_sources]

  @type t :: %__MODULE__{
    name: String.t,
    organisation: String.t,
    data_sources: [data_source]
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
  @spec start_link(cloak_id, %{String.t => any}) :: GenServer.server
  def start_link(cloak_id, cloak_info) do
    Air.ServiceRegistration.start_link(
          registration_key(cloak_id),
          registration_value(cloak_info),
          crash_on_error: true
        )
  end

  @doc "Returns the list of all connected cloaks and the associated metadata"
  @spec connected :: [t]
  def connected do
    for {key_path, _} <- :air_etcd.ls(@registration_root_key),
        {:ok, encoded_cloak_data} <- [:air_etcd.fetch("#{key_path}/main")],
        cloak_data = decode_cloak_data(encoded_cloak_data),
        # keeps false positives out, i.e. processes which have terminated, but the entry still lingers on
        Process.alive?(cloak_data.pid)
    do
      cloak_data.cloak_info
    end
  end

  @doc "Returns the pid of the main channel of the given cloak."
  @spec main_channel_pid(cloak_id) :: pid | nil
  def main_channel_pid(cloak_id) do
    case :air_etcd.fetch(registration_key(cloak_id)) do
      :error -> nil
      {:ok, encoded_value} ->
        encoded_value
        |> Base.decode64!()
        |> :erlang.binary_to_term()
        |> Map.fetch!(:pid)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp decode_cloak_data(encoded_cloak_data) do
    encoded_cloak_data
    |> Base.decode64!()
    |> :erlang.binary_to_term()
  end

  defp registration_key(cloak_id) do
    # base32 is used because the supported character set in the etcd key is limited
    "#{@registration_root_key}/#{Base.encode32(cloak_id)}/main"
  end

  defp registration_value(cloak_info) do
    %{
      pid: self(),
      cloak_info: parse_cloak_info(cloak_info)
    }
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  defp parse_cloak_info(cloak_info) do
    %__MODULE__{
      name: Map.fetch!(cloak_info, "name"),
      organisation: "unknown_org", # TODO: fix this when there's org info
      data_sources: Map.fetch!(cloak_info, "data_sources") |> parse_data_sources()
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
