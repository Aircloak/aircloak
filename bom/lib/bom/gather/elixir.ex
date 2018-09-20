defmodule BOM.Gather.Elixir do
  @moduledoc "Logic for reading elixir dependency information."

  alias BOM.{Gather, License}
  require Logger

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns the directory to include in the source zip and a list of packages contained in the given `deps` directory.
  """
  @spec run(String.t()) :: {String.t(), [Package.t()]}
  def run(deps_path) do
    version_map = version_map(deps_path)

    packages =
      deps_path
      |> Path.join("*")
      |> Path.wildcard()
      |> Enum.map(&{&1, version_map[package_name(&1)]})
      |> Enum.filter(fn {_, version} -> version end)
      |> Enum.map(&package/1)

    {deps_path, packages}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp package({path, {source, version}}) do
    %BOM.Package{
      realm: :elixir,
      name: package_name(path),
      path: path,
      license: license(source, path, version),
      version: version
    }
  end

  defp license(:hex, path, version),
    do:
      path
      |> hex_license(version)
      |> make_license(path, version)

  defp license(_not_from_hex, path, version) do
    case non_hex_license(package_name(path), version) do
      nil -> %License{type: :unknown, text: ""}
      type -> make_license(type, path, version)
    end
  end

  defp make_license(type, path, version),
    do:
      Gather.public_domain_license(type) || Gather.license_from_file(path, type) ||
        Gather.license_from_readme(path, type) || BOM.Whitelist.find(:elixir, package_name(path), version)

  defp hex_license(path, version) do
    case BOM.Gather.Elixir.Hex.licenses(package_name(path), version) do
      nil ->
        nil

      licenses ->
        licenses
        |> Enum.map(&License.name_to_type/1)
        |> Enum.find(&License.allowed_type?/1)
    end
  end

  defp version_map(deps_path) do
    Gather.if_matching_file(deps_path, "../mix.lock", fn text ->
      {deps, []} = text |> Code.format_string!() |> to_string() |> Code.eval_string()

      for {package, spec} <- deps, into: %{} do
        [source, _, version | _] = Tuple.to_list(spec)
        {to_string(package), {source, version}}
      end
    end)
  end

  defp package_name(path), do: Path.basename(path)

  for {package_name, version, license} <-
        [
          {"meck", "6121bb7e5c21b7ba2e79754f976a55f32940b28e", :apache2},
          {"pbkdf2", "7076584f5377e98600a7e2cb81980b2992fb2f71", :apache2},
          {"poison", "ca619d769815ab2c878cdfbf524c5b6890bcb000", :"cc0-1.0"},
          {"websocket_client", "c2a6cf11233cad54a7f7e6c89bca172f2b494f9d", :mit},
          {"tds", "91c3e9213c6e8c9b4a812555d16f55ed3b464bec", :apache2}
        ] do
    defp non_hex_license(unquote(package_name), unquote(version)), do: unquote(license)
  end

  defp non_hex_license(unknown_package, unknown_version) do
    if BOM.Whitelist.shipped?(:elixir, unknown_package),
      do: Logger.warn("unknown shipped non-hex dependency #{unknown_package} #{unknown_version}")

    nil
  end
end
