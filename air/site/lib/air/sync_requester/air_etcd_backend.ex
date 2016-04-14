defmodule Aircloak.SyncRequester.Backend.Etcd do
  @moduledoc """
  Etcd powered backend for `Aircloak.SyncRequester`.
  """

  @behaviour Aircloak.SyncRequester.Backend
  use GenServer
  require Logger

  # -------------------------------------------------------------------
  # Aircloak.SyncRequester.Backend callbacks
  # -------------------------------------------------------------------

  @doc false
  def store_request(path, id, request, meta) do
    :air_etcd.create_new(id_to_key(path, id), encode(request, meta))
  end

  @doc false
  def pop_request(path, id) do
    case :air_etcd.delete(id_to_key(path, id)) do
      {:ok, encoded_value} ->
        {request, meta} = decode(encoded_value)
        {:ok, request, meta}
      {:error, :not_found} ->
        {:error, :not_found}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp id_to_key(path, id) do
    # base32 is used because the supported character set in the etcd key is limited
    "#{path}/#{Base.encode32(id)}"
  end

  defp encode(request, meta) do
    {request, meta}
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  defp decode(encoded_value) do
    encoded_value
    |> Base.decode64!()
    |> :erlang.binary_to_term()
  end
end
