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
    license_from_file(path, "*{LICENSE,LICENCE,license,licence,License,License}*") ||
      license_from_readme(path, "*{README,readme,Readme}*") ||
      public_domain_license(path) ||
      BOM.Whitelist.find(:node, Path.basename(path))
  end

  defp license_from_file(path, pattern) do
    if_matching_file(path, pattern, fn text -> %License{type: :custom, text: text} end)
  end

  defp license_from_readme(path, pattern) do
    if_matching_file(path, pattern, fn text ->
      case Regex.run(~r/\n#* ?(license|licence)(.|\n)*/i, text) do
        [text | _]-> %License{type: :custom, text: text}
        _ -> nil
      end
    end)
  end

  defp public_domain_license(path) do
    case license_from_package_json(path) do
      "Public domain" -> License.find_by_type(:public_domain)
      "Public Domain" -> License.find_by_type(:public_domain)
      _ -> nil
    end
  end

  defp license_from_package_json(path) do
    if_matching_file(path, "package.json", fn text -> Poison.decode!(text)["license"] end)
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
