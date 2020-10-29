defmodule Mix.Tasks.Bom do
  @shortdoc "Generate bom.json"
  @usage """
    Usage:

      mix bom [--node <path>]+ [--elixir <path>]+ [--rust <path>]+ [--validate] <outdir>

      Add a --node switch for every node_modules directory to be searched. An yarn.lock file is
      assumed to exist at the same level as this directory.

      Add a --elixir switch for every elixir deps directory to be searched. A mix.lock file is assumed to
      exist at the same level as this directory.

      Add a --rust switch for every rust project to be searched. A Cargo.lock file is assumed to exist in that
      directory.

      If a --validate flag is passed, then errors will be shown when there are superflous entries in
      BOM.Whitelist or superflous licence files in priv/licenses. It only makes sense to pass this flag
      if all of the projects dependencies have been passed via the flags above. Note that validation can
      add a significant amount of time to this.

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
    case OptionParser.parse(args, strict: [node: :keep, elixir: :keep, rust: :keep, validate: :boolean]) do
      {opts, [outdir], []} ->
        do_run(Keyword.delete(opts, :validate), outdir, Keyword.get(opts, :validate, false))

      _ ->
        IO.puts(@usage)
        Mix.raise("Invalid usage")
    end
  end

  defp do_run(dirs, outdir, validate) do
    {:ok, _} = Application.ensure_all_started(:bom)

    IO.puts("Gathering package data...")
    {dirs, packages} = packages(dirs)
    IO.puts("Processing #{Enum.count(packages)} packages...")

    {invalid, valid} =
      packages
      |> Enum.filter(&BOM.Whitelist.shipped?(&1.realm, &1.name))
      |> Enum.map(&BOM.Whitelist.update_license_type/1)
      |> Enum.map(&BOM.Validate.run/1)
      |> Enum.split_with(& &1.error)

    if Enum.empty?(invalid) do
      if validate, do: do_validate(packages)

      json = Jason.encode!(valid)
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
      |> Enum.each(&IO.puts/1)

      Mix.raise("#{Enum.count(invalid)} invalid packages - see README.md for how to resolve this.")
    end
  end

  defp do_validate(packages) do
    case BOM.Whitelist.validate(packages) do
      :ok ->
        true

      %{whitelist: whitelist, licenses: licenses, digests: digests, not_shipped: not_shipped} ->
        unless Enum.empty?(whitelist) do
          IO.puts("Unnecessary packages found in BOM.Whitelist:")

          whitelist
          |> Enum.map(&"#{&1.name} #{&1.version} (#{&1.realm})")
          |> Enum.each(&IO.puts/1)
        end

        unless Enum.empty?(licenses) do
          IO.puts("Unnecessary license files found in priv/licenses:")

          licenses
          |> Enum.each(&IO.puts/1)
        end

        unless Enum.empty?(digests) do
          IO.puts("Unnecessary digests found in BOM.Whitelist:")

          digests
          |> Enum.each(&IO.puts/1)
        end

        unless Enum.empty?(not_shipped) do
          IO.puts("Unnecessary not_shipped keys found in BOM.Whitelist:")

          not_shipped
          |> Enum.each(&IO.puts/1)
        end

        Mix.raise("Please correct these issues and run again.")
    end
  end

  defp packages(dirs) do
    {dirs, packages} =
      dirs
      |> Enum.map(&do_packages/1)
      |> Enum.unzip()

    {dirs, packages |> Enum.concat() |> Enum.uniq_by(&{&1.realm, &1.name, &1.license})}
  end

  defp do_packages({:node, dir}), do: BOM.Gather.Node.run(dir)
  defp do_packages({:elixir, dir}), do: BOM.Gather.Elixir.run(dir)
  defp do_packages({:rust, dir}), do: BOM.Gather.Rust.run(dir)
end
