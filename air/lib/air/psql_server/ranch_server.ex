defmodule Air.PsqlServer.RanchServer do
  @moduledoc """
  Ranch powered TCP server which understands the PostgreSQL message protocol.

  This module can be used to serve PostgreSQL clients, such as the `psql`
  command-line tool, or ODBC drivers. The module is implemented as a behaviour
  which allows easy plugging of system specific logic. The module will drive
  the common workflow of PostgreSQL connection, invoking callback module's
  functions for specific tasks, such as authentication or query execution.
  __Note__: all callback functions are invoked in the connection process.
  """

  @behaviour :ranch_protocol
  use GenServer

  require Logger

  alias Air.PsqlServer.Protocol

  defstruct [:ref, :socket, :transport, :opts, :behaviour_mod, :protocol, :login_params, assigns: %{}]

  @type t :: %__MODULE__{
    # Only fields open to clients are specified here
    login_params: %{String.t => String.t},
    assigns: %{any => any}
  }

  @type opts :: [
    ssl: [:ssl.ssl_option],
    num_acceptors: pos_integer
  ]

  @type behaviour_init_arg :: any


  # -------------------------------------------------------------------
  # Behaviour callbacks
  # -------------------------------------------------------------------

  @doc "Invoked to allow callback module to initialize its state."
  @callback init(t, behaviour_init_arg) :: {:ok, t} | {:error, any}

  @doc "Invoked to login the user."
  @callback login(t, String.t) :: {:ok, t} | :error

  @doc "Invoked to run the query."
  @callback run_query(t, String.t, [Protocol.db_value], pos_integer) :: t

  @doc "Invoked to cancel a query on a backend."
  @callback cancel_query(t, ConnectionRegistry.key_data) :: t

  @doc "Invoked to describe the statement result."
  @callback describe_statement(t, String.t, [Protocol.db_value]) :: t

  @doc "Invoked when a message is received by the connection process."
  @callback handle_message(t, any) :: t


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the TCP server as the linked child of the caller process."
  @spec start_embedded_server(pos_integer, module, behaviour_init_arg, opts) :: Supervisor.on_start
  def start_embedded_server(port, behaviour_mod, behaviour_init_arg, opts \\ []) do
    opts = Keyword.merge([num_acceptors: 100], opts)
    Logger.info("Accepting PostgreSQL requests on port #{port}")
    :ranch_listener_sup.start_link(
      {__MODULE__, port},
      Keyword.fetch!(opts, :num_acceptors),
      :ranch_tcp,
      [port: port],
      __MODULE__, {opts, behaviour_mod, behaviour_init_arg}
    )
  end

  @doc "Stores an arbitrary key-value pair into a connection state."
  @spec assign(t, any, any) :: t
  def assign(conn, key, value), do:
    put_in(conn.assigns[key], value)

  @doc "Removes the value under the given key from the connection state."
  @spec unassign(t, any) :: t
  def unassign(conn, key), do:
    update_in(conn.assigns, &Map.delete(&1, key))

  @doc "Passes the query result to the protocol state."
  @spec query_result(t, Protocol.query_result) :: t
  def query_result(conn, query_result), do:
    update_protocol(conn, &Protocol.query_result(&1, query_result))

  @doc "Passes the query description to the protocol state."
  @spec describe_result(t, Protocol.describe_result) :: t
  def describe_result(conn, describe_result), do:
    update_protocol(conn, &Protocol.describe_result(&1, describe_result))

  @doc "Updates the protocol state."
  @spec update_protocol(t, ((Protocol.t) -> Protocol.t)) :: t
  def update_protocol(conn, fun), do:
    handle_protocol_actions(%__MODULE__{conn | protocol: fun.(conn.protocol)})


  # -------------------------------------------------------------------
  # :ranch_protocol callback functions
  # -------------------------------------------------------------------

  @impl :ranch_protocol
  def start_link(ref, socket, transport, {opts, behaviour_mod, behaviour_init_arg}), do:
    GenServer.start_link(__MODULE__, {ref, socket, transport, opts, behaviour_mod, behaviour_init_arg})


  # -------------------------------------------------------------------
  # GenServer callback functions
  # -------------------------------------------------------------------

  @impl GenServer
  def init({ref, socket, transport, opts, behaviour_mod, behaviour_init_arg}) do
    send(self(), {:after_init, behaviour_init_arg})
    {:ok, %__MODULE__{
      ref: ref,
      socket: socket,
      transport: transport,
      behaviour_mod: behaviour_mod,
      opts: opts,
      protocol: Protocol.new()
    }}
  end

  @impl GenServer
  def handle_info({:after_init, behaviour_init_arg}, conn) do
    :ok = :ranch.accept_ack(conn.ref)
    set_active_mode(conn)

    # We need to init behaviour after the connection has been accepted. Otherwise
    # in the case of an error, the client might hang forever.
    case conn.behaviour_mod.init(conn, behaviour_init_arg) do
      {:ok, conn} -> {:noreply, conn}
      {:error, reason} -> {:stop, reason, conn}
    end
  end
  def handle_info(:close, conn) do
    conn.transport.close(conn.socket)
    {:stop, :normal, conn}
  end
  for transport <- [:tcp, :ssl] do
    def handle_info({unquote(transport), _socket, input}, conn) do
      conn = update_protocol(conn, &Protocol.process(&1, input))
      set_active_mode(conn)
      {:noreply, conn}
    end
    def handle_info({unquote(:"#{transport}_closed"), _socket}, conn), do:
      {:stop, :normal, conn}
    def handle_info({unquote(:"#{transport}_error"), _socket, reason}, conn), do:
      {:stop, reason, conn}
  end
  def handle_info(:upgrade_to_ssl, conn) do
    with {:ok, ssl_opts} <- Keyword.fetch(conn.opts, :ssl),
         :ok <- conn.transport.setopts(conn.socket, active: false),
         {:ok, ssl_socket} = :ssl.ssl_accept(conn.socket, ssl_opts) do
      conn =
        update_protocol(
          %__MODULE__{conn | socket: ssl_socket, transport: :ranch_ssl},
          &Protocol.ssl_negotiated(&1)
        )

      :ok = set_active_mode(conn)
      {:noreply, conn}
    else
      _ -> {:stop, :ssl_error, conn}
    end
  end
  def handle_info(msg, conn), do:
    {:noreply, conn.behaviour_mod.handle_message(conn, msg)}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp set_active_mode(conn), do:
    conn.transport.setopts(conn.socket, active: :once)

  defp handle_protocol_actions(conn) do
    {actions, protocol} = Protocol.actions(conn.protocol)
    conn = %__MODULE__{conn | protocol: protocol}
    {output_chunks, other_actions} = extract_output_chunks(actions)
    conn.transport.send(conn.socket, output_chunks)
    case other_actions do
      [] -> conn
      _ -> Enum.reduce(other_actions, conn, &handle_protocol_action/2)
    end
  end

  defp extract_output_chunks(actions) do
    {send_actions, other_actions} = Enum.split_with(actions, &match?({:send, _}, &1))

    {
      Enum.map(send_actions, fn({:send, buffer}) -> buffer end),
      other_actions
    }
  end

  defp handle_protocol_action({:close, _reason}, conn) do
    send(self(), :close)
    conn
  end
  defp handle_protocol_action(:upgrade_to_ssl, conn) do
    send(self(), :upgrade_to_ssl)
    conn
  end
  defp handle_protocol_action({:login_params, login_params}, conn), do:
    conn
    |> Map.put(:login_params, login_params)
    |> update_protocol(&Protocol.authentication_method(&1, :cleartext))
  defp handle_protocol_action({:authenticate, password}, conn) do
    case conn.behaviour_mod.login(conn, password) do
      {:ok, conn} ->
        update_protocol(conn, &Protocol.authenticated(&1, true))
      :error ->
        update_protocol(conn, &Protocol.authenticated(&1, false))
    end
  end
  defp handle_protocol_action({:run_query, query, params, max_rows}, conn), do:
    conn.behaviour_mod.run_query(conn, query, params, max_rows)
  defp handle_protocol_action({:cancel_query, key}, conn), do:
    conn.behaviour_mod.cancel_query(conn, key)
  defp handle_protocol_action({:register_key_data, key_data}, conn), do:
    assign(conn, :key_data, key_data)
  defp handle_protocol_action({:describe_statement, query, params}, conn), do:
    conn.behaviour_mod.describe_statement(conn, query, params)


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec({port, behaviour_mod, behaviour_init_arg, opts}), do:
    Aircloak.ChildSpec.supervisor(
      __MODULE__, :start_embedded_server,
      [port, behaviour_mod, behaviour_init_arg, opts]
    )
end
