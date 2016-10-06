defmodule Air.PsqlServer.RanchServer do
  @moduledoc """
  Ranch powered TCP server which acts as a PostgreSQL server.

  This module can be used to serve PostgreSQL clients, such as the `psql`
  command-line tool, or ODBC drivers. Internally, the module is implemented as a
  driver of the `Air.PsqlServer.Protocol` state machine.
  """

  @behaviour :ranch_protocol
  use GenServer

  alias Air.{Repo, User}
  alias Air.PsqlServer.Protocol

  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns the supervisor specification for the TCP server on the given port."
  @spec supervisor_spec(pos_integer) :: Supervisor.child_spec
  def supervisor_spec(port), do:
    :ranch.child_spec(
      __MODULE__,
      100,
      :ranch_tcp, [port: port],
      __MODULE__, nil
    )


  #-----------------------------------------------------------------------------------------------------------
  # :ranch_protocol callback functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def start_link(ref, socket, transport, _arg), do:
    GenServer.start_link(__MODULE__, {ref, socket, transport})


  #-----------------------------------------------------------------------------------------------------------
  # GenServer callback functions
  #-----------------------------------------------------------------------------------------------------------

  @doc false
  def init({ref, socket, transport}) do
    send(self, :after_init)
    {:ok, %{
      ref: ref,
      socket: socket,
      transport: transport,
      protocol: Protocol.new(),
      login_params: %{}
    }}
  end

  @doc false
  def handle_info(:after_init, state) do
    :ok = :ranch.accept_ack(state.ref)
    set_active_mode(state)
    {:noreply, state}
  end
  def handle_info({:tcp, _socket, input}, state), do:
    {:noreply, process_input(state, input)}
  def handle_info({:tcp_closed, _socket}, state), do:
    {:stop, :normal, state}
  def handle_info({:tcp_error, _socket, reason}, state), do:
    {:stop, reason, state}
  def handle_info({:tcp_passive, _socket}, state) do
    set_active_mode(state)
    {:noreply, state}
  end
  def handle_info(:close, state) do
    state.transport.close(state.socket)
    {:stop, :normal, state}
  end
  def handle_info(_msg, state), do:
    {:noreply, state}


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp set_active_mode(state), do:
    :ok = state.transport.setopts(state.socket, active: 1)

  defp process_input(state, input), do:
    state
    |> update_protocol(&Protocol.process(&1, input))
    |> handle_protocol_actions()

  defp handle_protocol_actions(state) do
    {actions, protocol} = Protocol.actions(state.protocol)
    state = %{state | protocol: protocol}
    {output_chunks, other_actions} = extract_output_chunks(actions)
    state.transport.send(state.socket, output_chunks)
    case other_actions do
      [] -> state
      _ ->
        other_actions
        |> Enum.reduce(state, &handle_protocol_action/2)
        |> handle_protocol_actions()
    end
  end

  defp extract_output_chunks(actions) do
    {send_actions, other_actions} = Enum.partition(actions, &match?({:send, _}, &1))

    {
      Enum.map(send_actions, fn({:send, buffer}) -> buffer end),
      other_actions
    }
  end

  defp handle_protocol_action({:close, _reason}, state) do
    send(self(), :close)
    state
  end
  defp handle_protocol_action({:login_params, login_params}, state), do:
    state
    |> Map.put(:login_params, login_params)
    |> update_protocol(&Protocol.authentication_method(&1, :cleartext))
  defp handle_protocol_action({:authenticate, password}, state) do
    user = Repo.get_by(User, email: state.login_params["user"])
    update_protocol(state, &Protocol.authenticated(&1, User.validate_password(user, password)))
  end

  defp update_protocol(state, fun), do:
    %{state | protocol: fun.(state.protocol)}
end
