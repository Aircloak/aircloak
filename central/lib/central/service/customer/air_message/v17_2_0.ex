defmodule Central.Service.Customer.AirMessage.V17_2_0 do
  @moduledoc "Decoding and handling of messages sent by air in version 17.2.0"

  @doc false
  def handle(message, customer, air_name), do:
    Central.Service.Customer.AirMessage.Default.handle(message, customer, air_name)
end
