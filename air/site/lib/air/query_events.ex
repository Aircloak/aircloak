defmodule Air.QueryEvents do
  use GenEvent

  def start_link do
    GenEvent.start_link(name: __MODULE__)
  end

  def trigger_result(payload) do
    GenEvent.ack_notify(__MODULE__, {:result, payload})
  end

  def stream do
    GenEvent.stream(__MODULE__)
  end
end
