defmodule Central.CustomerMessage do
  @moduledoc "Decoding and handling of messages sent by air."
  require Logger
  alias Central.Repo
  alias Central.Service.Customer
  alias Central.Schemas.AirRPC


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Handles a message sent by the customer's air."
  @spec handle(map, Customer.t, String.t) :: any
  def handle(message, customer, air_name) do
    id = "#{air_name}|#{message["id"]}"
    case Repo.get(AirRPC, id) do
      nil ->
        # We look for event_payload for backwards compatibility with older airs
        payload = message["payload"] || message["event_payload"]
        result = handle(message["event"], payload, customer, air_name)
        changeset = AirRPC.changeset(%AirRPC{}, %{id: id, result: :erlang.term_to_binary(result)})
        Repo.insert!(changeset)
        result
      rpc ->
        Logger.info("Received a repeat RPC call for RPC id '#{id}'. The RPC was not re-executed. " <>
          "The type of the incoming RPC was '#{message["event"]}'")
        :erlang.binary_to_term(rpc.result)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp handle("query_execution", payload, customer, _air_name) do
    Logger.info("Received query execution update with payload: #{inspect payload}")
    customer = customer
    params = %{
      metrics: payload["metrics"],
      features: payload["features"],
      aux: payload["aux"],
    }
    Customer.record_query(customer, params)
  end
  defp handle("cloak_online", cloak_info, customer, air_name) do
    Central.Service.Customer.update_cloak(customer, air_name,
      Map.fetch!(cloak_info, "name"),
      status: :online, data_source_names: Map.get(cloak_info, "data_source_names", []),
        version: Map.get(cloak_info, "version", "Unknown")
    )
    :ok
  end
  defp handle("cloak_offline", cloak_info, customer, air_name) do
    Central.Service.Customer.update_cloak(customer, air_name,
      Map.fetch!(cloak_info, "name"), status: :offline)
    :ok
  end
  defp handle("usage_info", uptime_info, customer, air_name) do
    Central.Service.Customer.store_uptime_info(
      customer,
      air_name,
      NaiveDateTime.from_iso8601!(Map.fetch!(uptime_info, "air_utc_time")),
      Map.delete(uptime_info, "air_utc_time")
    )
    :ok
  end
  defp handle(other, data, _customer, _air_name) do
    Logger.warn("unknown call `#{other}` (#{inspect(data)})")
    # Responding with ok, because the client can't fix this issue by retrying
    :ok
  end
end
