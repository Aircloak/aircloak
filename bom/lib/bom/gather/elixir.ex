defmodule BOM.Gather.Elixir do
  @moduledoc "Logic for reading elixir dependency information."

  alias BOM.{Gather,License}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns a list of packages contained in the given `deps` directory."
  @spec run(String.t) :: [Package.t]
  def run(deps_path) do
    version_map = version_map(deps_path)

    deps_path
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.map(&{&1, version_map[package_name(&1)]})
    |> Enum.filter(fn {_, version} -> version end)
    |> Enum.map(&package/1)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp package({path, {source, version}}) do
    %BOM.Package{
      realm: :elixir,
      name: package_name(path),
      path: path,
      license: license(path, source, version),
      version: version,
    }
  end

  defp license(path, source, version) do
    type = license_type(source, path, version)

    Gather.public_domain_license(type) ||
      Gather.license_from_file(path, type) ||
      Gather.license_from_readme(path, type) ||
      BOM.Whitelist.find(:elixir, package_name(path), version)
  end

  defp license_type(:hex, path, version) do
    case BOM.Gather.Elixir.Hex.licenses(package_name(path), version) do
      nil -> nil
      licenses ->
        licenses
        |> Enum.map(&License.name_to_type/1)
        |> Enum.find(&License.allowed_type?/1)
    end
  end
  defp license_type(_other, _path, _version), do:
    # It's not a hex dependency, so we can't determine the version from hex
    nil

  defp version_map(deps_path) do
    Gather.if_matching_file(deps_path, "../mix.lock", fn text ->
      {deps, []} = Code.eval_string(text)

      for {package, spec} <- deps, into: %{} do
        [source, _, version | _] = Tuple.to_list(spec)
        {to_string(package), {source, version}}
      end
    end)
  end

  defp package_name(path), do: Path.basename(path)
end
