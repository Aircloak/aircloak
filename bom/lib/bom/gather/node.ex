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
      license_from_file(path, "*license*") ||
      license_from_file(path, "*licence*") ||
      license_from_readme(path, "*README*") ||
      BOM.Whitelist.find(:node, Path.basename(path))
  end

  defp license_from_file(path, pattern) do
    if_matching_file(path, pattern, fn text -> %License{type: :custom, text: text} end)
  end

  defp license_from_readme(path, pattern) do
    if_matching_file(path, pattern, fn text ->
      case Regex.run(~r/LICENSE(.|\n)*/, text) do
        [text | _]-> %License{type: :custom, text: text}
        _ -> nil
      end
    end)
  end

  defp if_matching_file(path, pattern, action) do
    path
    |> Path.join(pattern)
    |> Path.wildcard()
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> nil
      matching_path ->
        File.read(matching_path)
        |> case do
          {:ok, text} -> action.(text)
          _ -> nil
        end
    end
  end
end
