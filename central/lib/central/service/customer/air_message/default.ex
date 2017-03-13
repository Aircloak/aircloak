defmodule Central.Service.Customer.AirMessage.Default do
  @moduledoc "Decoding and handling of messages sent by air pre-versioning."
  require Logger
  alias Central.Service.Customer
  alias Central.{Repo, Schemas.AirRPC}

  known_messages = ~w(query_execution cloak_online cloak_offline usage_info)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Handles an Air message"
  @spec handle(Customer.AirMessage.rpc, Central.Schemas.Customer.t, String.t) :: :ok | :error
  def handle(message, customer, air_name) do
    message_id = Map.fetch!(message, "id")
    case rpc(customer, air_name, message_id) do
      nil ->
        result = do_handle(message, customer, air_name)
        store_rpc!(customer, air_name, message_id, result)
        result

      rpc ->
        Logger.info("Received a repeat RPC call for RPC id '#{rpc.id}'. The RPC was not re-executed. " <>
          "The type of the incoming RPC was '#{message["event"]}'")
        :erlang.binary_to_term(rpc.result)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  for message_name <- known_messages, function_name = :"handle_#{message_name}" do
    def do_handle(%{"event" => unquote(message_name)} = message, customer, air_name) do
      # We look for event_payload for backwards compatibility with older airs
      payload = message["payload"] || message["event_payload"]
      unquote(function_name)(%{payload: payload, customer: customer, air_name: air_name})
      :ok
    end
  end
  def do_handle(unknown_message) do
    Logger.error("unknown air message: #{inspect unknown_message}")
    :error
  end

  defp handle_query_execution(message) do
    Logger.info("Received query execution update with payload: #{inspect message.payload}")
    params = %{
      metrics: message.payload["metrics"],
      features: message.payload["features"],
      aux: message.payload["aux"],
    }
    Customer.record_query(message.customer, params)
  end

  defp handle_cloak_online(message), do:
    Central.Service.Customer.update_cloak(message.customer, message.air_name,
      Map.fetch!(message.payload, "name"),
      status: :online, data_source_names: Map.get(message.payload, "data_source_names", []),
        version: Map.get(message.payload, "version", "Unknown")
    )

  defp handle_cloak_offline(message), do:
    Central.Service.Customer.update_cloak(message.customer, message.air_name,
      Map.fetch!(message.payload, "name"), status: :offline)

  defp handle_usage_info(message), do:
    Central.Service.Customer.store_uptime_info(
      message.customer,
      message.air_name,
      NaiveDateTime.from_iso8601!(Map.fetch!(message.payload, "air_utc_time")),
      Map.delete(message.payload, "air_utc_time")
    )

  defp rpc(customer, air_name, message_id), do:
    Repo.get(AirRPC, rpc_id(customer, air_name, message_id))

  defp store_rpc!(customer, air_name, message_id, result), do:
    %AirRPC{}
    |> AirRPC.changeset(%{id: rpc_id(customer, air_name, message_id), result: :erlang.term_to_binary(result)})
    |> Repo.insert!()

  defp rpc_id(customer, air_name, message_id), do:
    Enum.join([customer.id, air_name, message_id], "|")
end
