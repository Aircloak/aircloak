defmodule BOM.Whitelist do
  @moduledoc "Contains information about packages for which a license or its type could not be automatically found."

  alias BOM.{License, Package}


  # -------------------------------------------------------------------
  # Whitelists
  # -------------------------------------------------------------------

  @licenses %{
    :elixir => %{
      {"ecto",            "2.2.6"}  => %{type: :apache2, text: :provided},
      {"erlware_commons", "0.21.0"} => %{type: :mit,     text: :provided},
      {"scrivener",       "2.3.0"}  => %{type: :mit,     text: :standard},
      {"scrivener_ecto",  "1.2.2"}  => %{type: :mit,     text: :standard},
      {"scrivener_html",  "1.3.3"}  => %{type: :mit,     text: :standard},
      {"excoveralls",     "0.5.7"}  => %{type: :mit,     text: :standard},
      {"ecto_enum",       "1.0.2"}  => %{type: :mit,     text: :provided},
      {"file_system",     "0.2.2"}  => %{type: :wtfpl,   text: :standard},
    },
    :node => %{
      {"base64-js",                 "0.0.2"}  => %{type: :mit,           text: :provided},
      {"bcrypt-pbkdf",              "1.0.0"}  => %{type: :bsd_4_clause,  text: :standard},
      {"bit-twiddle",               "0.0.2"}  => %{type: :mit,           text: :provided},
      {"browser-stdout",            "1.3.0"}  => %{type: :isc,           text: :standard},
      {"browserify-cipher",         "1.0.0"}  => %{type: :mit,           text: :standard},
      {"browserify-des",            "1.0.0"}  => %{type: :mit,           text: :standard},
      {"brunch",                    "2.8.2"}  => %{type: :mit,           text: :provided},
      {"brunch-skeletons",          "0.1.4"}  => %{type: :mit,           text: :standard},
      {"cipher-base",               "1.0.3"}  => %{type: :mit,           text: :standard},
      {"create-ecdh",               "4.0.0"}  => %{type: :mit,           text: :standard},
      {"create-hash",               "1.1.2"}  => %{type: :mit,           text: :standard},
      {"create-hmac",               "1.1.4"}  => %{type: :mit,           text: :standard},
      {"csscolorparser",            "1.0.3"}  => %{type: :mit,           text: :provided},
      {"damerau-levenshtein",       "1.0.3"}  => %{type: :bsd_2_clause,  text: :standard},
      {"deppack",                   "0.7.0"}  => %{type: :mit,           text: :provided},
      {"diffie-hellman",            "5.0.2"}  => %{type: :mit,           text: :standard},
      {"esprima",                   "1.1.1"}  => %{type: :bsd_2_clause,  text: :provided},
      {"esrecurse",                 "4.1.0"}  => %{type: :bsd_2_clause,  text: :standard},
      {"evp_bytestokey",            "1.0.0"}  => %{type: :mit,           text: :standard},
      {"fcache",                    "0.1.1"}  => %{type: :isc,           text: :standard},
      {"findup-sync",               "0.4.2"}  => %{type: :mit,           text: :provided},
      {"geojson-rewind",            "0.1.0"}  => %{type: :bsd_2_clause,  text: :standard},
      {"gl-line2d",                 "1.3.0"}  => %{type: :mit,           text: :standard},
      {"glsl-read-float",           "1.1.0"}  => %{type: :mit,           text: :standard},
      {"glslify-deps",              "1.3.0"}  => %{type: :isc,           text: :standard},
      {"invariant",                 "2.2.1"}  => %{type: :bsd_3_clause,  text: :standard},
      {"json-schema",               "0.2.2"}  => %{type: :bsd_3_clause,  text: :provided},
      {"json-schema",               "0.2.3"}  => %{type: :bsd_3_clause,  text: :provided},
      {"jsonlint-lines-primitives", "1.6.0"}  => %{type: :mit,           text: :provided},
      {"kdbush",                    "1.0.1"}  => %{type: :isc,           text: :standard},
      {"loggy",                     "0.3.5"}  => %{type: :mit,           text: :provided},
      {"loose-envify",              "1.2.0"}  => %{type: :mit,           text: :standard},
      {"mapbox-gl-shaders",         "1.0.0"}  => %{type: :isc,           text: :standard},
      {"mapbox-gl-supported",       "1.2.0"}  => %{type: :bsd_3_clause,  text: :standard},
      {"micro-promisify",           "0.1.1"}  => %{type: :mit,           text: :provided},
      {"minimalistic-assert",       "1.0.0"}  => %{type: :isc,           text: :standard},
      {"object-keys",               "0.4.0"}  => %{type: :mit,           text: :provided},
      {"parse-asn1",                "5.0.0"}  => %{type: :isc,           text: :standard},
      {"point-geometry",            "0.0.0"}  => %{type: :isc,           text: :provided},
      {"public-encrypt",            "4.0.0"}  => %{type: :mit,           text: :standard},
      {"quickselect",               "1.0.0"}  => %{type: :isc,           text: :standard},
      {"randombytes",               "2.0.3"}  => %{type: :mit,           text: :standard},
      {"react-onclickoutside",      "4.9.0"}  => %{type: :mit,           text: :standard},
      {"regenerator-runtime",       "0.9.5"}  => %{type: :bsd_2_clause,  text: :provided},
      {"simplicial-complex",        "0.3.3"}  => %{type: :mit,           text: :provided},
      {"skemata",                   "0.1.2"}  => %{type: :mit,           text: :standard},
      {"source-map",                "0.4.4"}  => %{type: :bsd_3_clause,  text: :provided},
      {"tweetnacl",                 "0.14.3"} => %{type: :public_domain, text: :standard},
      {"union-find",                "0.0.4"}  => %{type: :mit,           text: :provided},
      {"unitbezier",                "0.0.0"}  => %{type: :bsd_3_clause,  text: :standard},
      {"weak-map",                  "1.0.5"}  => %{type: :apache2,       text: :provided},
      {"wgs84",                     "0.0.0"}  => %{type: :bsd_2_clause,  text: :standard},
      {"wordwrap",                  "0.0.2"}  => %{type: :mit,           text: :provided},
      {"pagedown",                  "1.1.0"}  => %{type: :mit,           text: :provided},
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
    "8de5f23be471b6814f19b2ad82a5208a" => :mit,           # node/path-is-inside
    "ffcf739dca268cb0f20336d6c1a038f1" => :apache2,       # node/rc
    "99d097ff2dae4db019dd8ac5144f1efc" => :mit,           # node/commander
    "330031db3ec2b47f6e9d7923b8e1f95b" => :mit,           # node/color-convert
    "6d2716539b6e3fee1dff17903670f1cd" => :mit,           # node/react-chartjs-2
    "73ecf9f2f7acdcafcc3eae4d585362de" => :apache2,       # elixir/earmark
    "0b36f89594d6a8a4b5e8efa73f1f4fc5" => :mit,           # elixir/fs
    "0689a7b07fec79946ae192573e1450e8" => :bsd_3_clause,  # elixir/getopt
    "9741c346eef56131163e13b9db1241b3" => :mpl_2_0,       # elixir/jose
    "4d8e2e181d7f8cdc38226f5ee04e5fdd" => :mit,           # elixir/phoenix_gen_socket_server
    "d0d1fe59ece5018a431ad8e694ec6c6a" => :wtfpl,         # elixir/file_system
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a License struct for the given package if a license for `package` in `realm` has been manually
  checked and whitelisted, an unknown license otherwise.
  """
  @spec find(atom, String.t, String.t) :: License.t | nil
  def find(realm, package, version) do
    key = {package, version}
    if Map.has_key?(@licenses[realm], key) do
      license(realm, package, @licenses[realm][key])
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
    cond do
      License.allowed_type?(license.type) -> package
      License.empty?(license) -> package
      true -> %{package | license: %{license | type: type_by_text(license.text)}}
    end
  end

  @not_shipped %{
    elixir: ~w(proper triq excheck),
    node: ~w(
      eslint eslint-config-airbnb eslint-plugin-import eslint-plugin-jsx-a11y eslint-plugin-react
      eslint-config-airbnb-base eslint-import-resolver-node
    ),
  }

  @doc """
  Returns false if the given package is used only for tests or building and not shipped with the product, true
  otherwise.
  """
  @spec shipped?(atom, String.t) :: boolean
  def shipped?(realm, name) do
    not_shipped = Map.get(@not_shipped, realm, [])
    !Enum.member?(not_shipped, name)
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
