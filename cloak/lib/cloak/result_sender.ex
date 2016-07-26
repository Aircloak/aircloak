defmodule Cloak.ResultSender do
  @moduledoc "Handles returning the result of a query back to the requester"

  @type target :: {:process, pid()} | :air_socket

  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @doc false
  def supervisor_spec do
    import Supervisor.Spec
    supervisor(Supervisor, [
      [worker(__MODULE__, [], restart: :transient)],
      [name: __MODULE__, strategy: :simple_one_for_one, max_restarts: 10, max_seconds: 10]
    ], [id: __MODULE__])
  end

  @doc false
  def start_link(target, reply) do
    Task.start_link(fn -> send_reply(target, reply) end)
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Sends the reply to the target. Uses a normal process send if target is `{:process, pid}`. Uses
  the Air <-> Cloak socket if it's :air_socket.
  """
  @spec send_result(target(), term()) :: :ok
  def send_result(target, reply) do
    {:ok, _} = Supervisor.start_child(__MODULE__, [target, reply])
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp send_reply(:air_socket, reply) do
    Elixir.Cloak.AirSocket.send_query_result(reply)
  end
  defp send_reply({:process, pid}, reply) do
    send(pid, {:reply, reply})
  end
end
