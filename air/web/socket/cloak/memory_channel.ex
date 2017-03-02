defmodule Air.Socket.Cloak.MemoryChannel do
  @moduledoc "Channel for the cloak to report the amount of memory it has to the air."
  use Phoenix.Channel
  require Logger


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  @dialyzer {:nowarn_function, join: 3} # Phoenix bug, fixed in master
  def join("memory_channel", _cloak_info, socket) do
    Logger.info("Joined memory channel")
    {:ok, socket}
  end

  @doc false
  @dialyzer {:nowarn_function, terminate: 2} # Phoenix bug, fixed in master
  def terminate(_reason, socket) do
    {:ok, socket}
  end

  @doc false
  @dialyzer {:nowarn_function, handle_in: 3} # Phoenix bug, fixed in master
  def handle_in("reading", reading, socket) do
    Air.Service.Cloak.record_memory(reading)
    Air.Socket.Frontend.MemoryChannel.broadcast_memory_reading(socket.assigns.cloak_id, reading)
    {:noreply, socket}
  end
  def handle_in(event, _payload, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.warn("unknown event #{event} from '#{cloak_id}'")
    {:noreply, socket}
  end

  @doc false
  @dialyzer {:nowarn_function, handle_info: 2} # Phoenix bug, fixed in master
  def handle_info(message, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("unhandled info #{inspect(message)} from '#{cloak_id}'")
    {:noreply, socket}
  end
end
