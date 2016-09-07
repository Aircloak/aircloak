defmodule Mix.Tasks.Bom do
  @shortdoc "Generate bom.json"
  @usage """
    Usage:

      mix bom [--node <path>]+ [--elixir <path>]+ <output>

      Add a --node switch for every node_modules directory to be searched.
      Add a --elixir switch for every elixir deps directory to be searched.
      The file will be generated at <output>
  """

  @moduledoc "Gathers information about dependencies and outputs a bom.json file into the _build directory.\n\n" <>
    @usage

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  def run(args) do
    case OptionParser.parse(args, strict: [node: :keep, elixir: :keep]) do
      {dirs, [output], []} -> do_run(dirs, output)
      _ ->
        IO.puts(@usage)
        Mix.raise("Invalid usage")
    end
  end

  defp do_run(dirs, output) do
    {:ok, _} = Application.ensure_all_started(:bom)

    {invalid, valid} = packages(dirs)
    |> Enum.filter(&BOM.Whitelist.shipped?/1)
    |> Enum.map(&BOM.Whitelist.update_license_type/1)
    |> Enum.map(&BOM.Validate.run/1)
    |> Enum.partition(&(&1.error))

    if Enum.empty?(invalid) do
      json = Poison.encode!(valid)
      File.write!(output, json)
      :ok
    else
      invalid
      |> Enum.map(&"#{&1.name}: #{&1.error}")
      |> Enum.map(&IO.puts/1)

      Mix.raise("#{Enum.count(invalid)}/#{Enum.count(valid) + Enum.count(invalid)} invalid packages")
    end
  end

  defp packages(dirs), do: Enum.flat_map(dirs, &do_packages/1)

  defp do_packages({:node, dir}), do: BOM.Gather.Node.run(dir)
  defp do_packages({:elixir, dir}), do: BOM.Gather.Elixir.run(dir)
end
