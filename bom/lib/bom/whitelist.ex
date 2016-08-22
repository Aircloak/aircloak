defmodule BOM.Whitelist do
  @licenses %{
    :node => %{
      "JSV" => :custom,
    }
  }

  def find(realm, package) do
    if Map.has_key?(@licenses[realm], package) do
      license(realm, package, @licenses[realm][package])
    else
      nil
    end
  end

  defp license(realm, package, type) do
    text = "licenses" |> Path.join(to_string(realm)) |> Path.join(package) |> File.read!()
    %BOM.License{type: type, text: text}
  end
end
