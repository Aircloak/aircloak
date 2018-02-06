defmodule Mix.Tasks.Bom do
  @shortdoc "Generate bom.json"
  @usage """
    Usage:

      mix bom [--node <path>]+ [--elixir <path>]+ <outdir>

      Add a --node switch for every node_modules directory to be searched. An yarn.lock file is
      assumed to exist at the same level as this directory.

      Add a --elixir switch for every elixir deps directory to be searched. A mix.lock file is assumed to
      exist at the same level as this directory.

      In the <outdir> directory, the following files will be generated:
      - bom.json: contains the bill of material in JSON format
      - dependencies.zip: contains a copy of all the dependency source files
  """

  @moduledoc "Gathers information about dependencies and outputs a bom.json file into the _build directory.\n\n" <>
    @usage

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  def run(args) do
    case OptionParser.parse(args, strict: [node: :keep, elixir: :keep]) do
      {dirs, [outdir], []} -> do_run(dirs, outdir)
      _ ->
        IO.puts(@usage)
        Mix.raise("Invalid usage")
    end
  end

  defp do_run(dirs, outdir) do
    {:ok, _} = Application.ensure_all_started(:bom)

    IO.puts("Gathering package data...")
    packages = packages(dirs)
    IO.puts("Processing #{Enum.count(packages)} packages...")

    {invalid, valid} = packages
    |> Enum.filter(&BOM.Whitelist.shipped?(&1.realm, &1.name))
    |> Enum.map(&BOM.Whitelist.update_license_type/1)
    |> Enum.map(&BOM.Validate.run/1)
    |> Enum.split_with(&(&1.error))

    if Enum.empty?(invalid) do
      json = Poison.encode!(valid)
      bom_file_path = Path.join([outdir, "bom.json"])
      File.write!(bom_file_path, json)
      IO.puts("Bill of Materials written to #{bom_file_path}")

      IO.puts("Packaging dependency sources...")
      bom_source_path = Path.join([outdir, "dependencies.zip"])
      BOM.Gather.Sources.collect(dirs, bom_source_path)
      IO.puts("Dependency sources written to #{bom_source_path}")
    else
      invalid
      |> Enum.map(&"#{&1.name} #{&1.version} (#{&1.path}): #{&1.error}")
      |> Enum.map(&IO.puts/1)

      Mix.raise("#{Enum.count(invalid)} invalid packages - see README.md for how to resolve this.")
    end
  end

  defp packages(dirs) do
    dirs
    |> Enum.flat_map(&do_packages/1)
    |> Enum.uniq_by(&{&1.realm, &1.name, &1.license})
  end

  defp do_packages({:node, dir}), do: BOM.Gather.Node.run(dir)
  defp do_packages({:elixir, dir}), do: BOM.Gather.Elixir.run(dir)
end
