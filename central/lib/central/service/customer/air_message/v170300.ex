defmodule Central.Service.Customer.AirMessage.V170300 do
  @moduledoc "Decoding and handling of messages sent by air in version 17.3.0"

  @doc false
  def handle(message, customer, air_name, options), do:
    Central.Service.Customer.AirMessage.Default.handle(message, customer, air_name, options)
end
