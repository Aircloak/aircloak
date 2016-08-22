defmodule Mix.Tasks.Bom do
  use Mix.Task

  def run(_args) do
    "../air/site/node_modules"
    |> BOM.Gather.node()
    |> IO.inspect
  end
end
