defmodule Mix.Tasks.Bom do
  use Mix.Task

  def run(_args) do
    invalid = "../air/site/node_modules"
    |> BOM.Gather.node()
    |> Enum.map(&BOM.Validate.run/1)
    |> Enum.filter(&(&1.error))

    invalid
    |> Enum.map(&"#{&1.name}: #{&1.error}")
    |> Enum.map(&IO.puts/1)

    IO.puts("#{Enum.count(invalid)} invalid packages")
  end
end
