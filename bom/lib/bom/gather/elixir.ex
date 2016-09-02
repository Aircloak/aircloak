defmodule BOM.Gather.Elixir do
  alias BOM.{Gather,License}

  def run(deps_path) do
    deps_path
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.map(&package/1)
  end

  defp package(path) do
    %BOM.Package{
      realm: :elixir,
      name: package_name(path),
      license: license(path),
    }
  end

  defp license(path) do
    license_from_file(path) ||
      BOM.Whitelist.find(:elixir, package_name(path))
  end

  defp license_from_file(path), do:
    Gather.if_matching_file(path, Gather.license_files(), fn(text) -> %License{type: license_type(path), text: text} end)

  defp license_type(path) do
    {200, package, _http_headers} = path |> package_name() |> Hex.API.Package.get()
    %{"meta" => %{"licenses" => licenses}} = package

    (licenses || [])
    |> Enum.map(&License.name_to_type/1)
    |> Enum.find(&License.allowed_type?/1)
  end

  defp package_name(path), do: Path.basename(path)
end
