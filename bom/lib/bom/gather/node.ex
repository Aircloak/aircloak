defmodule BOM.Gather.Node do
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
    with package_path = Path.join(path, "/package.json"),
         {:ok, json} <- File.read(package_path),
         {:ok, package_description} <- Poison.decode(json)
    do
      package_description["license"] || package_description["licenses"] || license_from_file(path)
    else
      _ -> license_from_file(path)
    end
  end

  defp license_from_file(path) do
    path
    |> Path.join("*LICENSE*")
    |> Path.wildcard()
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> nil
      license_path ->
        File.read(license_path)
        |> case do
          {:ok, text} -> %BOM.License{type: :custom, text: text}
          _ -> nil
        end
    end
  end
end
