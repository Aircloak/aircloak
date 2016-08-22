defmodule BOM.License do
  defstruct [:type, :text]

  @types %{
    "MIT" => :mit,
    "BSD" => :bsd_3_clause,
    "BSD-2-Clause" => :bsd_2_clause,
    "BSD-3-Clause" => :bsd_3_clause,
    "Apache 2.0" => :apache2,
    "ISC" => :isc,
    "Public domain" => :public_domain,
    "Public Domain" => :public_domain,
  }

  def find_by_name(name) do
    if Map.has_key?(@types, name) do
      {:ok, %__MODULE__{type: @types[name], text: text(@types[name])}}
    else
      {:error, :unknown_license}
    end
  end

  for path <- Path.wildcard("./licenses/*") do
    name = Path.basename(path) |> String.to_atom
    text = File.read!(path)

    defp text(unquote(name)) do
      unquote(text)
    end
  end
end
