defmodule BOM.License do
  defstruct [:type, :text]
end

defmodule BOM.Package do
  defstruct [:type, :name, :license]
end

defmodule BOM.Gather do
  def node(path) do
    path
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.map(&node_package/1)
  end

  defp node_package(path) do
    %BOM.Package{
      type: :node,
      name: Path.basename(path),
      license: node_license(path)
    }
  end

  defp node_license(path) do
    path
    |> Path.join("/LICENSE")
    |> File.read()
    |> case do
      {:ok, text} -> %BOM.License{text: text}
      {:error, _} -> nil
    end
  end
end
