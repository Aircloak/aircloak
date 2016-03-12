defmodule Air.PeerDiscovery do
  @moduledoc """
  Module for setting up the discovery of peers.
  """

  use GenServer
  require Logger

  @poll_interval :timer.seconds(10)
  @nodes_etcd_root "/service_instances/insights_erlang_nodes"


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates the supervisor specification for peer discovery.

  Inserting this into some supervisor will create necessary processes which register
  this node, and search and connect to other peers.
  """
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec do
    import Supervisor.Spec, warn: false

    children = [
      worker(__MODULE__, []),
      worker(Air.ServiceRegistration, node_registration_args(), id: :node_registration)
    ]
    supervisor(Supervisor, [children, [strategy: :one_for_all]], id: Module.concat(__MODULE__, Supervisor))
  end


  # -------------------------------------------------------------------
  # Internal API
  # -------------------------------------------------------------------

  @doc false
  def start_link,
    do: GenServer.start_link(__MODULE__, nil)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    Process.flag(:trap_exit, true)
    :net_kernel.monitor_nodes(true)
    Process.send_after(self(), :poll_peers, 0)
    {:ok, nil}
  end

  @doc false
  def handle_info(:poll_peers, nil) do
    {:ok, poller} = Task.start_link(&poll_peers/0)
    {:noreply, poller}
  end
  def handle_info({:EXIT, poller, _}, poller) do
    Process.send_after(self(), :poll_peers, @poll_interval)
    {:noreply, nil}
  end
  def handle_info({:nodeup, node}, poller) do
    Logger.info(fn -> "Connected to #{node}" end)
    {:noreply, poller}
  end
  def handle_info({:nodedown, node}, poller) do
    Logger.info(fn -> "Disconnected from #{node}" end)
    {:noreply, poller}
  end
  def handle_info(_, poller), do: {:noreply, poller}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp poll_peers do
    for {_key, node_str} <- :air_etcd.ls(@nodes_etcd_root),
        peer_node = String.to_atom(node_str),
        not Enum.member?(Node.list([:this, :visible]), peer_node) do
      unless Node.connect(peer_node), do: Logger.warn(fn -> "Failed connecting to peer #{peer_node}" end)
    end
    :ok
  end

  defp node_registration_args do
    node_string = "#{node()}"
    sanitized_node_string = String.replace(node_string, "@", "_")
    [
      "#{@nodes_etcd_root}/#{sanitized_node_string}",
      node_string
    ]
  end
end
