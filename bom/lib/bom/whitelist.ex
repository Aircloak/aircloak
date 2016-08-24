defmodule BOM.Whitelist do
  @licenses %{
    :node => %{
      "JSV" => %{type: :bsd_2_clause, text: :provided},
      "wgs84" => %{type: :bsd_2_clause, text: :standard},
    }
  }

  def find(realm, package) do
    if Map.has_key?(@licenses[realm], package) do
      license(realm, package, @licenses[realm][package])
    else
      nil
    end
  end

  defp license(realm, package, %{type: type, text: :provided}) do
    text = "licenses" |> Path.join(to_string(realm)) |> Path.join(package) |> File.read!()
    %BOM.License{type: type, text: text}
  end
  defp license(_realm, _package, %{type: type, text: :standard}) do
    BOM.License.find_by_type(type)
  end
end
