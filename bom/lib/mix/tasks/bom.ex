defmodule Mix.Tasks.Bom do
  @shortdoc "Generate bom.json"
  @moduledoc "Gathers information about dependencies and outputs a bom.json file into the _build directory."

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  def run(_args) do
    {:ok, _} = Application.ensure_all_started(:bom)

    {invalid, valid} = packages()
    |> Enum.filter(&BOM.Whitelist.shipped?/1)
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

      Mix.raise("#{Enum.count(invalid)}/#{Enum.count(valid) + Enum.count(invalid)} invalid packages")
    end
  end

  defp packages do
    BOM.Gather.Node.run("../air/site/node_modules") ++
      BOM.Gather.Elixir.run("../air/site/deps")
  end
end
