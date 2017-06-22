defmodule Cloak.ResultSender do
  @moduledoc "Handles returning the result of a query back to the requester"

  @type target :: {:process, pid()} | :air_socket
  @type query_state :: :parsing | :compiling | :awaiting_data | :ingesting_data | :processing | :post_processing

  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @doc false
  def supervisor_spec do
    import Supervisor.Spec
    supervisor(Supervisor, [
      [worker(__MODULE__, [], restart: :temporary)],
      [name: __MODULE__, strategy: :simple_one_for_one, max_restarts: 10, max_seconds: 10]
    ], [id: __MODULE__])
  end

  @doc false
  def start_link(target, type, payload) do
    Task.start_link(fn -> send_reply(target, type, payload) end)
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Sends a query state update to the target. Uses a normal process send if target is `{:process, pid}`.
  Uses the Air <-> Cloak socket if it's :air_socket.
  """
  @spec send_state(target(), String.t, query_state()) :: :ok
  def send_state(target, query_id, query_state) do
    {:ok, _} = Supervisor.start_child(__MODULE__, [target, :state, {query_id, query_state}])
    :ok
  end

  @doc """
  Sends the reply to the target. Uses a normal process send if target is `{:process, pid}`.
  Uses the Air <-> Cloak socket if it's :air_socket.
  """
  @spec send_result(target(), term()) :: :ok
  def send_result(target, reply) do
    {:ok, _} = Supervisor.start_child(__MODULE__, [target, :result, reply])
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp send_reply(:air_socket, :result, reply) do
    case Elixir.Cloak.AirSocket.send_query_result(reply) do
      :ok -> :ok
      {:error, %Poison.EncodeError{}} ->
        reply = %{error: "Result could not be encoded as JSON.", query_id: reply.query_id}
        Elixir.Cloak.AirSocket.send_query_result(reply)
      {:error, error} -> {:error, error}
    end
  end
  defp send_reply(:air_socket, :state, {query_id, query_state}), do:
    Elixir.Cloak.AirSocket.send_query_state(query_id, query_state)
  defp send_reply({:process, pid}, type, reply), do:
    send(pid, {type, reply})
end
