defmodule BOM.License do
  defstruct [:type, :text]

  def find_by_type(type) do
    %__MODULE__{type: type, text: text(type)}
  end

  for path <- Path.wildcard("./licenses/generic/*") do
    name = Path.basename(path) |> String.to_atom
    text = File.read!(path)

    defp text(unquote(name)) do
      unquote(text)
    end
  end
end
