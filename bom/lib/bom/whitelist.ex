defmodule BOM.Whitelist do
  @moduledoc "Contains information about packages for which a license or its type could not be automatically found."

  alias BOM.{License, Package}


  # -------------------------------------------------------------------
  # Whitelists
  # -------------------------------------------------------------------

  @licenses %{
    :elixir => %{
      "ecto" => %{type: :apache2, text: :provided},
      "erlware_commons" => %{type: :mit, text: :provided},
      "excoveralls" => %{type: :mit, text: :standard},
    },
    :node => %{
      "jsv"                         => %{type: :bsd_2_clause, text: :provided},
      "browserify-cipher"           => %{type: :mit,          text: :standard},
      "browserify-des"              => %{type: :mit,          text: :standard},
      "brunch"                      => %{type: :mit,          text: :provided},
      "brunch-skeletons"            => %{type: :mit,          text: :standard},
      "cipher-base"                 => %{type: :mit,          text: :standard},
      "create-ecdh"                 => %{type: :mit,          text: :standard},
      "create-hash"                 => %{type: :mit,          text: :standard},
      "create-hmac"                 => %{type: :mit,          text: :standard},
      "csscolorparser"              => %{type: :mit,          text: :provided},
      "deppack"                     => %{type: :mit,          text: :provided},
      "diffie-hellman"              => %{type: :mit,          text: :standard},
      "envify"                      => %{type: :mit,          text: :standard},
      "eslint-config-airbnb"        => %{type: :mit,          text: :provided},
      "eslint-config-airbnb-base"   => %{type: :mit,          text: :provided},
      "eslint-import-resolver-node" => %{type: :mit,          text: :provided},
      "esrecurse"                   => %{type: :bsd_2_clause, text: :standard},
      "evp_bytestokey"              => %{type: :mit,          text: :standard},
      "fcache"                      => %{type: :isc,          text: :standard},
      "geojson-rewind"              => %{type: :bsd_2_clause, text: :standard},
      "gl-line2d"                   => %{type: :mit,          text: :standard},
      "glsl-read-float"             => %{type: :mit,          text: :standard},
      "glslify-deps"                => %{type: :isc,          text: :standard},
      "invariant"                   => %{type: :bsd_3_clause, text: :standard},
      "json-schema"                 => %{type: :bsd_3_clause, text: :provided},
      "jsonlint-lines-primitives"   => %{type: :mit,          text: :provided},
      "kdbush"                      => %{type: :isc,          text: :standard},
      "loggy"                       => %{type: :mit,          text: :provided},
      "loose-envify"                => %{type: :mit,          text: :standard},
      "mapbox-gl-js-supported"      => %{type: :bsd_3_clause, text: :standard},
      "micro-promisify"             => %{type: :mit,          text: :provided},
      "minimalistic-assert"         => %{type: :isc,          text: :standard},
      "mousetrap"                   => %{type: :apache2,      text: :standard},
      "parse-asn1"                  => %{type: :isc,          text: :standard},
      "point-geometry"              => %{type: :isc,          text: :provided},
      "public-encrypt"              => %{type: :mit,          text: :standard},
      "randombytes"                 => %{type: :mit,          text: :standard},
      "react-onclickoutside"        => %{type: :mit,          text: :standard},
      "skemata"                     => %{type: :mit,          text: :standard},
      "unitbezier"                  => %{type: :bsd_3_clause, text: :standard},
      "weak-map"                    => %{type: :apache2,      text: :provided},
      "wgs84"                       => %{type: :bsd_2_clause, text: :standard},
    }
  }

  @type_by_text_digest %{
    "c06db4b145ce991f7e579f17699fdf2f" => :mit,           # node/amdefine
    "864383f6d0b46747d1d580ef2fc2f67a" => :isc,           # node/babel-brunch
    "a4ae3515249a7180a4af2a7be17636d9" => :mit,           # node/colors
    "95cc4f9fe9d1c095151534b92c412a9c" => :mit,           # node/css-brunch
    "bc3c23d98d7aa86bbf232058884e19b2" => :mit,           # node/extsprintf
    "478909a701ade1b289d6e548fc9a7999" => :isc,           # node/feature-filter
    "df1ee3f3e3f8585543aca8ab319c7d8e" => :mit,           # node/growl
    "950e018e87c0d974cc09cdc1aed56da1" => :mit,           # node/indexof
    "2be2157b55ea281b7f4969d7ba05eea2" => :mit,           # node/ms
    "0d8c303f84b56d8c334cffb5e6df6444" => :mit,           # node/nomnom
    "5fc2c6d40f1d589b9530cbec8b857263" => :mit,           # node/numeric
    "9dcefced2116bbfa2c3ea64b8f5dbbc2" => :mit,           # node/phoenix
    "1dc701356996e3d0dd135248577c8ef7" => :mit,           # node/phoenix_html
    "eacbaae25552d53aba44661c68b770d7" => :mit,           # node/progress
    "c8307a7b7a1394f77e887475cf03cd1d" => :bsd_3_clause,  # node/rw
    "62212b2d5d003ee7f76e89c7d15ef00e" => :public_domain, # node/tv4
    "44348b65b421f5f075c74680c11786d4" => :mit,           # node/uglify-js-brunch
    "7c26dfe36e38a743a435b92f7e1260af" => :apache2,       # elixir/earmark
    "0b36f89594d6a8a4b5e8efa73f1f4fc5" => :mit,           # elixir/fs
    "0689a7b07fec79946ae192573e1450e8" => :bsd_3_clause,  # elixir/getopt
    "9567c64d58e18a81951c75d00c10fa98" => :epl_1_1,       # elixir/gproc
    "9741c346eef56131163e13b9db1241b3" => :mpl_2_0,       # elixir/jose
    "4d8e2e181d7f8cdc38226f5ee04e5fdd" => :mit,           # elixir/phoenix_gen_socket_server
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a License struct for the given package if a license for `package` in `realm` has been manually
  checked and whitelisted, nil otherwise.
  """
  @spec find(atom, String.t) :: License.t | nil
  def find(realm, package) do
    if Map.has_key?(@licenses[realm], package) do
      license(realm, package, @licenses[realm][package])
    else
      License.unknown()
    end
  end

  @doc "Returns the license for all babel packages (node.js package family)."
  @spec babel_license :: License.t
  def babel_license do
    %License{type: :mit, text: get_text(:node, "babel")}
  end

  @doc """
  Returns the package with its license type set if its license has been whitelisted and classified. Otherwise
  sets the license type to `{:unknown, digest}` - it will need to be manually classified and the digest
  whitelisted if we can use that license.

  Does not change packages which have been automatically determined to have a valid license.
  """
  @spec update_license_type(Package.t) :: Package.t
  def update_license_type(package = %Package{license: license}) do
    if License.allowed_type?(license.type) do
      package
    else
      %{package | license: %{license | type: type_by_text(license.text)}}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp type_by_text(text) do
    digest = digest(text)
    @type_by_text_digest[digest] || {:unknown, digest}
  end

  defp digest(text) do
    text
    |> :erlang.bitstring_to_list()
    |> :erlang.md5()
    |> :erlang.bitstring_to_list()
    |> Enum.map(&(:io_lib.format("~2.16.0b", [&1])))
    |> List.flatten()
    |> to_string()
  end

  defp license(realm, package, %{type: type, text: :provided}) do
    %License{type: type, text: get_text(realm, package)}
  end
  defp license(_realm, _package, %{type: type, text: :standard}) do
    License.find_by_type(type)
  end

  defp get_text(realm, package) do
    "licenses" |> Path.join(to_string(realm)) |> Path.join(package) |> File.read!()
  end
end
