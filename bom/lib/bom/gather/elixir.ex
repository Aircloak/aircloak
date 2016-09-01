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
      name: Path.basename(path),
      license: license(path),
    }
  end

  defp license(path) do
    license_from_file(path) ||
      BOM.Whitelist.find(:elixir, Path.basename(path))
  end

  defp license_from_file(path), do:
    Gather.if_matching_file(path, Gather.license_files(), fn(text) -> %License{type: nil, text: text} end)
end
