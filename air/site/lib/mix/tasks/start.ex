defmodule Mix.Tasks.Start do
  @shortdoc "Starts the site for local development inside an iex session"
  @moduledoc """
  Starts the site for local development inside an iex session.

  Usage:

  ```
  mix start [node_suffix]
  ```

  `node_suffix` is an integer index of the node. All started nodes will be
  connected in a local cluster.
  """
  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  @doc false
  def run(args) do
    {_, node_arg, []} = OptionParser.parse(args)

    node_suffix =
      case node_arg do
        [suffix_str] -> String.to_integer(suffix_str)
        _ -> 1
      end

    # Turn the instance into a node
    {:ok, _} = :net_kernel.start([:"insights#{node_suffix}@127.0.0.1", :longnames])

    # start iex
    Application.ensure_all_started(:iex)
    IEx.start

    # start the site
    configure_port(node_suffix - 1)
    Mix.Task.run("phoenix.server")

    # need to sleep forever to keep the shell running
    :timer.sleep(:infinity)
  end

  defp configure_port(port_offset) do
    endpoint_config = Application.get_env(:air, Air.Endpoint, [])
    if port_offset > 0 do
      runtime_endpoint_config = update_in(endpoint_config, [:http, :port],
          fn(port_base) -> port_base + port_offset end)
      Application.put_env(:air, Air.Endpoint, runtime_endpoint_config, persistent: true)
    end
  end
end
