defmodule Central.Service.Customer.AirMessage.V180210 do
  @moduledoc "Decoding and handling of messages sent by air in version 18.2.1"
  # credo:disable-for-this-file Credo.Check.Design.DuplicatedCode

  require Logger
  alias Central.Service.Customer

  known_messages = ~w(query_execution)

  @type options :: [check_duplicate_rpc?: boolean]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Handles an Air message"
  @spec handle(Customer.AirMessage.rpc(), Central.Schemas.Customer.t(), String.t(), options) ::
          :ok | {:error, :duplicate_rpc}
  def handle(message, customer, air_name, options) do
    options = Keyword.merge([check_duplicate_rpc?: true], options)
    message_id = Map.fetch!(message, "id")

    if check_duplicate_rpc?(options) && Customer.rpc_imported?(customer, air_name, message_id) do
      Logger.info("Received a repeated RPC call. The RPC was not re-executed.")
      {:error, :duplicate_rpc}
    else
      result = do_handle(message, customer, air_name)
      if check_duplicate_rpc?(options), do: Customer.store_rpc!(customer, air_name, message_id)
      result
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp check_duplicate_rpc?(options), do: Keyword.fetch!(options, :check_duplicate_rpc?)

  for message_name <- known_messages, function_name = :"handle_#{message_name}" do
    def do_handle(%{"event" => unquote(message_name)} = message, customer, air_name) do
      # We look for event_payload for backwards compatibility with older airs
      payload = message["payload"] || message["event_payload"]
      unquote(function_name)(%{payload: payload, customer: customer, air_name: air_name})
      :ok
    end
  end

  def do_handle(unknown_message, customer, air_name) do
    Logger.error(
      "unknown air message: #{inspect(unknown_message)} from #{inspect(customer)} and air: " <> "#{inspect(air_name)}"
    )

    :error
  end

  defp handle_query_execution(message) do
    Logger.debug(fn ->
      "Received query execution update for customer: #{message.customer.name}."
    end)

    params = %{
      metrics: message.payload["metrics"],
      features: message.payload["features"],
      aux: message.payload["aux"]
    }

    Customer.record_query(message.customer, params)
  end
end
