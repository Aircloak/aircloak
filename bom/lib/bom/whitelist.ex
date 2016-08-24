defmodule BOM.Whitelist do
  @licenses %{
    :node => %{
      "JSV" => %{type: :bsd_2_clause, text: :provided},
      "browserify-cipher" => %{type: :mit, text: :standard},
      "browserify-des" => %{type: :mit, text: :standard},
      "brunch" => %{type: :mit, text: :provided},
      "brunch-skeletons" => %{type: :mit, text: :standard},
      "cipher-base" => %{type: :mit, text: :standard},
      "create-ecdh" => %{type: :mit, text: :standard},
      "create-hash" => %{type: :mit, text: :standard},
      "create-hmac" => %{type: :mit, text: :standard},
      "csscolorparser" => %{type: :mit, text: :provided},
      "deppack" => %{type: :mit, text: :provided},
      "diffie-hellman" => %{type: :mit, text: :standard},
      "envify" => %{type: :mit, text: :standard},
      "eslint-config-airbnb" => %{type: :mit, text: :provided},
      "eslint-config-airbnb-base" => %{type: :mit, text: :provided},
      "eslint-import-resolver-node" => %{type: :mit, text: :provided},
      "esrecurse" => %{type: :bsd_2_clause, text: :standard},
      "evp_bytestokey" => %{type: :mit, text: :standard},
      "fcache" => %{type: :isc, text: :standard},
      "geojson-rewind" => %{type: :bsd_2_clause, text: :standard},
      "gl-line2d" => %{type: :mit, text: :standard},
      "glsl-read-float" => %{type: :mit, text: :standard},
      "glslify-deps" => %{type: :isc, text: :standard},
      "invariant" => %{type: :bsd_3_clause, text: :standard},
      "json-schema" => %{type: :bsd_3_clause, text: :provided},
      "jsonlint-lines-primitives" => %{type: :mit, text: :provided},
      "kdbush" => %{type: :isc, text: :standard},
      "loggy" => %{type: :mit, text: :provided},
      "loose-envify" => %{type: :mit, text: :standard},
      "mapbox-gl-js-supported" => %{type: :bsd_3_clause, text: :standard},
      "micro-promisify" => %{type: :mit, text: :provided},
      "minimalistic-assert" => %{type: :isc, text: :standard},
      "mousetrap" => %{type: :apache2, text: :standard},
      "parse-asn1" => %{type: :isc, text: :standard},
      "point-geometry" => %{type: :isc, text: :provided},
      "public-encrypt" => %{type: :mit, text: :standard},
      "randombytes" => %{type: :mit, text: :standard},
      "react-onclickoutside" => %{type: :mit, text: :standard},
      "skemata" => %{type: :mit, text: :standard},
      "unitbezier" => %{type: :bsd_3_clause, text: :standard},
      "weak-map" => %{type: :apache2, text: :provided},
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

  def babel_license do
    %BOM.License{type: :mit, text: get_text(:node, "babel")}
  end

  defp license(realm, package, %{type: type, text: :provided}) do
    %BOM.License{type: type, text: get_text(realm, package)}
  end
  defp license(_realm, _package, %{type: type, text: :standard}) do
    BOM.License.find_by_type(type)
  end

  defp get_text(realm, package) do
    "licenses" |> Path.join(to_string(realm)) |> Path.join(package) |> File.read!()
  end
end
