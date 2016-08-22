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
    path
    |> Path.join("/LICENSE")
    |> File.read()
    |> case do
      {:ok, text} -> %BOM.License{text: text}
      {:error, _} -> nil
    end
  end
end
