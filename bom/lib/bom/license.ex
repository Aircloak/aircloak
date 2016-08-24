defmodule BOM.License do
  defstruct [:type, :text]

  def find_by_type(type) do
    %__MODULE__{type: type, text: text(type)}
  end

  def name_to_type("MIT"), do: :mit
  def name_to_type("MIT/X11"), do: :mit
  def name_to_type("ISC"), do: :isc
  def name_to_type("FreeBSD"), do: :bsd_3_clause
  def name_to_type("BSD"), do: :bsd_3_clause
  def name_to_type("Apache License, Version 2.0"), do: :apache2
  def name_to_type(_), do: :unknown

  for path <- Path.wildcard("./licenses/generic/*") do
    name = Path.basename(path) |> String.to_atom
    text = File.read!(path)

    defp text(unquote(name)) do
      unquote(text)
    end

    def allowed_type?(unquote(name)), do: true
  end
  def allowed_type?(_), do: false
end
