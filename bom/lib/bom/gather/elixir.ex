defmodule BOM.Gather.Elixir do
  @moduledoc "Logic for reading elixir dependency information."

  alias BOM.{Gather,License}
  require Logger

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
      license: license(source, path, version),
      version: version,
    }
  end

  defp license(:hex, path, version), do:
    path
    |> hex_license(version)
    |> make_license(path, version)
  defp license(_not_from_hex, path, version) do
    case non_hex_license(package_name(path), version) do
      nil -> %License{type: :unknown, text: ""}
      type -> make_license(type, path, version)
    end
  end

  defp make_license(type, path, version), do:
    Gather.public_domain_license(type) ||
    Gather.license_from_file(path, type) ||
    Gather.license_from_readme(path, type) ||
    BOM.Whitelist.find(:elixir, package_name(path), version)

  defp hex_license(path, version) do
    case BOM.Gather.Elixir.Hex.licenses(package_name(path), version) do
      nil -> nil
      licenses ->
        licenses
        |> Enum.map(&License.name_to_type/1)
        |> Enum.find(&License.allowed_type?/1)
    end
  end

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

  for {package_name, version, license} <-
    [
      {"websocket_client", "16cd139a71d99813ec0c0cd1d8cb92dbde93d9c7", :mit},
      {"meck", "dde759050eff19a1a80fd854d7375174b191665d", :apache2},
      {"pbkdf2", "7076584f5377e98600a7e2cb81980b2992fb2f71", :apache2},
      {"poison", "1a6bff505c22047e18a9318e01bda63ede20d649", :"cc0-1.0"},
    ]
  do
    defp non_hex_license(unquote(package_name), unquote(version)), do:
      unquote(license)
  end
  defp non_hex_license(unknown_package, unknown_version) do
    if BOM.Whitelist.shipped?(:elixir, unknown_package), do:
      Logger.warn("unknown shipped non-hex dependency #{unknown_package} #{unknown_version}")

    nil
  end
end
