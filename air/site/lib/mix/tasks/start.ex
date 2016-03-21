defmodule Mix.Tasks.Start do
  @shortdoc "Starts the site for local development"
  @moduledoc """
  Starts the site for local development.

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

    # start the site
    configure_port(node_suffix - 1)
    Mix.Task.run("phoenix.server")
  end

  defp configure_port(port_offset) do
    if port_offset > 0 do
      Air.Utils.update_app_env(:air, Air.Endpoint, [persistent: true],
          &update_in(&1, [:http, :port], fn(port_base) -> port_base + port_offset end))
    end
  end
end
