defmodule Central.Service.Customer.AirMessage.Default do
  @moduledoc "Decoding and handling of messages sent by air pre-versioning."
  require Logger
  alias Central.Service.Customer

  known_messages = ~w(query_execution cloak_online cloak_offline usage_info)

  @type options :: [check_rpc?: boolean]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Handles an Air message"
  @spec handle(Customer.AirMessage.rpc, Central.Schemas.Customer.t, String.t, options) ::
    :ok | {:error, :duplicate_rpc}
  def handle(message, customer, air_name, options) do
    options = Keyword.merge([check_rpc?: true], options)
    message_id = Map.fetch!(message, "id")
    if check_rpc?(options) && Customer.rpc(customer, air_name, message_id) != nil do
      Logger.info("Received a repeated RPC call. The RPC was not re-executed.")
      {:error, :duplicate_rpc}
    else
      result = do_handle(message, customer, air_name)
      if check_rpc?(options), do:
        Customer.store_rpc!(customer, air_name, message_id, result)
      result
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp check_rpc?(options), do:
    Keyword.fetch!(options, :check_rpc?)

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
end
