defmodule Air.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel
  require Logger


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("main", cloak_info, socket) do
    cloak_id = socket.assigns.cloak_id

    # Using `ServiceRegistration` for strongly consistent discovery of the channel process.
    {:ok, _} = Air.ServiceRegistration.start_link(
          registration_key(cloak_id),
          registration_value(),
          crash_on_error: true
        )

    {:ok, %{}, assign(socket, :cloak_info, cloak_info)}
  end

  @doc false
  def terminate(_reason, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("cloak '#{cloak_id}' left air")
    {:ok, socket}
  end

  @doc false
  def handle_in(event, _payload, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.warn("unknown event #{event} from '#{cloak_id}'")
    {:noreply, socket}
  end

  @doc false
  def handle_info(message, socket) do
    cloak_id = socket.assigns.cloak_id
    Logger.info("unhandled info #{message} from '#{cloak_id}'")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp registration_key(cloak_id) do
    # base16 is used because the supported character set in the key is limited
    "/settings/air/cloaks/#{Base.encode16(cloak_id)}/main"
  end

  defp registration_value do
    self() |> :erlang.term_to_binary() |> Base.encode64()
  end
end
