defmodule Mix.Tasks.Bom do
  use Mix.Task

  def run(_args) do
    {invalid, valid} = "../air/site/node_modules"
    |> BOM.Gather.node()
    |> Enum.map(&BOM.Whitelist.update_license_type/1)
    |> Enum.map(&BOM.Validate.run/1)
    |> Enum.partition(&(&1.error))

    if Enum.empty?(invalid) do
      json = Poison.encode!(valid)
      File.write!("_build/bom.json", json)
      :ok
    else
      invalid
      |> Enum.map(&"#{&1.name}: #{&1.error}")
      |> Enum.map(&IO.puts/1)

      Mix.raise("#{Enum.count(invalid)} invalid packages")
    end
  end
end
