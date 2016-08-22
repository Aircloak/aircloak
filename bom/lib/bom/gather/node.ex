defmodule BOM.Gather.Node do
  alias BOM.License

  def run(path) do
    path
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.map(&package/1)
  end

  defp package(path) do
    %BOM.Package{
      type: :node,
      name: Path.basename(path),
      license: license(path)
    }
  end

  defp license(path) do
    license_from_file(path, "*LICENSE*") ||
      license_from_file(path, "*LICENCE*") ||
      with package_path = Path.join(path, "/package.json"),
           {:ok, json} <- File.read(package_path),
           {:ok, package_description} <- Poison.decode(json),
           {:ok, license} <- License.find_by_name(package_description["license"])
      do
        license
      else
        _ -> nil
      end
  end

  defp license_from_file(path, pattern) do
    path
    |> Path.join(pattern)
    |> Path.wildcard()
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> nil
      license_path ->
        File.read(license_path)
        |> case do
          {:ok, text} -> %License{type: :custom, text: text}
          _ -> nil
        end
    end
  end
end
