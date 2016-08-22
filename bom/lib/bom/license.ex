defmodule BOM.License do
  defstruct [:type, :text]

  def find_by_name("MIT"), do: %__MODULE__{type: :mit, text: text(:mit)}
  def find_by_name("BSD-2-Clause"), do: %__MODULE__{type: :bsd_2_clause, text: text(:bsd_2_clause)}
  def find_by_name("Apache 2.0"), do: %__MODULE__{type: :apache2, text: text(:apache2)}
  def find_by_name(_), do: %__MODULE__{type: :unknown, text: ""}

  for path <- Path.wildcard("./licenses/*") do
    name = Path.basename(path) |> String.to_atom
    text = File.read!(path)

    defp text(unquote(name)) do
      unquote(text)
    end
  end
end
