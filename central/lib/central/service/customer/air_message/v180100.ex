defmodule Central.Service.Customer.AirMessage.V180100 do
  @moduledoc "Decoding and handling of messages sent by air in version 18.1.0"

  @doc false
  def handle(message, customer, air_name, options), do:
    Central.Service.Customer.AirMessage.Default.handle(message, customer, air_name, options)
end
