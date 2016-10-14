defmodule Air.PsqlServer.RanchServer do
  @moduledoc """
  Ranch powered TCP server which understands the PostgreSQL message protocol.

  This module can be used to serve PostgreSQL clients, such as the `psql`
  command-line tool, or ODBC drivers. Internally, the module is implemented as a
  driver of the `Air.PsqlServer.Protocol` state machine.
  """

  @behaviour :ranch_protocol
  use GenServer

  require Logger

  alias Air.PsqlServer.Protocol

  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc "Returns the supervisor specification for the TCP server."
  @spec supervisor_spec() :: Supervisor.child_spec
  def supervisor_spec() do
    port = Application.fetch_env!(:air, Air.PsqlServer)[:port]
    Logger.info("Accepting PostgreSQL requests on port #{port}")
    :ranch.child_spec(
      __MODULE__,
      100,
      :ranch_tcp, [port: port],
      __MODULE__, nil
    )
  end


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
  def handle_info(:close, state) do
    state.transport.close(state.socket)
    {:stop, :normal, state}
  end
  for transport <- [:tcp, :ssl] do
    def handle_info({unquote(transport), _socket, input}, state) do
      state = process_input(state, input)
      set_active_mode(state)
      {:noreply, state}
    end
    def handle_info({unquote(:"#{transport}_closed"), _socket}, state), do:
      {:stop, :normal, state}
    def handle_info({unquote(:"#{transport}_error"), _socket, reason}, state), do:
      {:stop, reason, state}
  end
  def handle_info(:upgrade_to_ssl, state) do
    state.transport.setopts(state.socket, active: false)
    {:ok, ssl_socket} = :ssl.ssl_accept(
      state.socket,
      certfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_cert.pem"]),
      keyfile: Path.join([Application.app_dir(:air, "priv"), "config", "ssl_key.pem"])
    )

    state =
      update_protocol(
        %{state | socket: ssl_socket, transport: :ranch_ssl},
        &Protocol.ssl_negotiated(&1)
      )

    :ok = set_active_mode(state)
    {:noreply, state}
  end
  def handle_info(_msg, state), do:
    {:noreply, state}


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp set_active_mode(state), do:
    state.transport.setopts(state.socket, active: true)

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
  defp handle_protocol_action(:upgrade_to_ssl, state) do
    send(self(), :upgrade_to_ssl)
    state
  end
  defp handle_protocol_action({:login_params, login_params}, state), do:
    state
    |> Map.put(:login_params, login_params)
    |> update_protocol(&Protocol.authentication_method(&1, :cleartext))
  defp handle_protocol_action({:authenticate, password}, state), do:
    update_protocol(
      state,
      &Protocol.authenticated(&1, authenticated?(state.login_params, password))
    )
  defp handle_protocol_action({:run_query, query}, state) do
    Logger.debug("Running query: `#{query}`")
    update_protocol(state, &Protocol.select_result(&1, []))
  end

  defp authenticated?(login_params, password), do:
    match?({:ok, _}, Air.Service.User.login(login_params["user"], password, login_params["database"]))

  defp update_protocol(state, fun), do:
    %{state | protocol: fun.(state.protocol)}
end
