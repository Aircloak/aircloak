defmodule Air.Socket.Cloak.MainChannel do
  @moduledoc """
  Main communication channel between a cloak and the air system.
  """
  use Phoenix.Channel


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join(_topic, _join_payload, socket),
    do: {:ok, %{}, socket}

  @doc false
  def handle_in(_event, _payload, socket),
    do: {:noreply, socket}

  @doc false
  def handle_info(_message, socket),
    do: {:noreply, socket}
end
