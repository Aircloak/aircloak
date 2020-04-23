defmodule BOM.Whitelist do
  @moduledoc "Contains information about packages for which a license or its type could not be automatically found."

  alias BOM.{License, Package}

  # -------------------------------------------------------------------
  # Whitelists
  # -------------------------------------------------------------------

  @licenses %{
    :elixir => %{
      {"ecto_enum", "1.0.2"} => %{type: :mit, text: :provided},
      {"excoveralls", "0.5.7"} => %{type: :mit, text: :standard},
      {"file_system", "0.2.7"} => %{type: :wtfpl, text: :standard},
      {"makeup_elixir", "0.14.0"} => %{type: :bsd_4_clause, text: :standard},
      {"scrivener_ecto", "2.2.0"} => %{type: :mit, text: :provided},
      {"scrivener", "2.7.0"} => %{type: :mit, text: :provided}
    },
    :node => %{
      {"spdx-exceptions", "2.2.0"} => %{type: :cca3u, text: :standard}
    },
    :rust => %{
      {"cfg-if", "0.1.5"} => %{type: :mit, text: :provided},
      {"log", "0.4.5"} => %{type: :mit, text: :provided},
      {"odbc-safe", "0.4.1"} => %{type: :mit, text: :provided},
      {"odbc-sys", "0.6.3"} => %{type: :mit, text: :provided},
      {"odbc", "0.9.12"} => %{type: :mit, text: :provided},
      {"simple-error", "0.1.11"} => %{type: :mit, text: :provided}
    }
  }

  @type_by_text_digest %{
    # node/cli-table
    "742167dd80c0afaa048b5fc2d45ca499" => :mit,
    # node/caniuse-db
    "eacc0b19e3fb8dd12d2e110b24be0452" => :cca4i,
    # node/type-fest
    "915042b5df33c31a6db2b37eadaa00e3" => :mit,
    # node/pako
    "a4f08d6b2d1bf3f3a1bc296a6109a25b" => :mit,
    # node/caniuse-lite
    "60f8103054954b2c75f1faa295ea3590" => :cca4i,
    # node/atob
    "493adefc1fe80b079a23203e4732d945" => :mit,
    # node/amdefine
    "c06db4b145ce991f7e579f17699fdf2f" => :mit,
    # node/babel-brunch
    "864383f6d0b46747d1d580ef2fc2f67a" => :isc,
    # node/colors
    "a4ae3515249a7180a4af2a7be17636d9" => :mit,
    # node/css-brunch
    "95cc4f9fe9d1c095151534b92c412a9c" => :mit,
    # node/extsprintf
    "bc3c23d98d7aa86bbf232058884e19b2" => :mit,
    # node/feature-filter
    "478909a701ade1b289d6e548fc9a7999" => :isc,
    # node/growl
    "df1ee3f3e3f8585543aca8ab319c7d8e" => :mit,
    # node/indexof
    "950e018e87c0d974cc09cdc1aed56da1" => :mit,
    # node/ms
    "2be2157b55ea281b7f4969d7ba05eea2" => :mit,
    # node/nomnom
    "0d8c303f84b56d8c334cffb5e6df6444" => :mit,
    # node/numeric
    "5fc2c6d40f1d589b9530cbec8b857263" => :mit,
    # node/phoenix
    "9dcefced2116bbfa2c3ea64b8f5dbbc2" => :mit,
    # node/phoenix_html
    "1dc701356996e3d0dd135248577c8ef7" => :mit,
    # node/progress
    "eacbaae25552d53aba44661c68b770d7" => :mit,
    # node/rw
    "c8307a7b7a1394f77e887475cf03cd1d" => :bsd_3_clause,
    # node/tv4
    "62212b2d5d003ee7f76e89c7d15ef00e" => :public_domain,
    # node/uglify-js-brunch
    "44348b65b421f5f075c74680c11786d4" => :mit,
    # node/path-is-inside
    "8de5f23be471b6814f19b2ad82a5208a" => :mit,
    # node/rc
    "ffcf739dca268cb0f20336d6c1a038f1" => :apache2,
    # node/commander
    "99d097ff2dae4db019dd8ac5144f1efc" => :mit,
    # node/color-convert
    "330031db3ec2b47f6e9d7923b8e1f95b" => :mit,
    # node/react-chartjs-2
    "6d2716539b6e3fee1dff17903670f1cd" => :mit,
    # node/ua-parser-js
    "c8a186b02a48de60f8df66ba326360a2" => :mit,
    # elixir/fs
    "0b36f89594d6a8a4b5e8efa73f1f4fc5" => :mit,
    # elixir/getopt
    "0689a7b07fec79946ae192573e1450e8" => :bsd_3_clause,
    # elixir/jose
    "9741c346eef56131163e13b9db1241b3" => :mpl_2_0,
    # elixir/phoenix_gen_socket_server
    "4d8e2e181d7f8cdc38226f5ee04e5fdd" => :mit,
    # elixir/file_system
    "d0d1fe59ece5018a431ad8e694ec6c6a" => :wtfpl,
    # elixir/idna
    "7c9b6269d40a09414db760aa524bf240" => :mit,
    # elixir/earmark
    "1bf8028080e75e094cd7b53003c2efeb" => :apache2
  }

  @not_shipped %{
    elixir: ~w(proper triq excheck rustler),
    node: ~w(
      eslint eslint-config-airbnb eslint-plugin-import eslint-plugin-jsx-a11y eslint-plugin-react
      eslint-config-airbnb-base eslint-import-resolver-node
    )
  }

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a License struct for the given package if a license for `package` in `realm` has been manually
  checked and whitelisted, an unknown license otherwise.
  """
  @spec find(atom, String.t(), String.t()) :: License.t() | nil
  def find(realm, package, version) do
    key = {package, version}

    if Map.has_key?(@licenses[realm], key) do
      license(realm, package, @licenses[realm][key])
    else
      License.unknown()
    end
  end

  @doc "Returns the license for all babel packages (node.js package family)."
  @spec babel_license :: License.t()
  def babel_license do
    %License{type: :mit, text: get_text(:node, "babel")}
  end

  @doc """
  Returns the package with its license type set if its license has been whitelisted and classified. Otherwise
  sets the license type to `{:unknown, digest}` - it will need to be manually classified and the digest
  whitelisted if we can use that license.

  Does not change packages which have been automatically determined to have a valid license.
  """
  @spec update_license_type(Package.t()) :: Package.t()
  def update_license_type(package = %Package{license: license}) do
    cond do
      License.allowed_type?(license.type) -> package
      License.empty?(license) -> package
      true -> %{package | license: %{license | type: type_by_text(license.text)}}
    end
  end

  @doc """
  Returns false if the given package is used only for tests or building and not shipped with the product, true
  otherwise.
  """
  @spec shipped?(atom, String.t()) :: boolean
  def shipped?(realm, name) do
    not_shipped = Map.get(@not_shipped, realm, [])
    !Enum.member?(not_shipped, name)
  end

  @doc """
  Validates if this module or priv/licences contain superflous entries.
  This is to make sure this file doesn't grow indefinitely.
  """
  @spec validate([Package.t()]) ::
          :ok
          | %{
              whitelist: [%{realm: :atom, name: String.t(), license: String.t(), version: String.t()}],
              licenses: [String.t()],
              digests: [String.t()],
              not_shipped: [String.t()]
            }
  def validate(packages) do
    whitelist = validate_whitelist(packages)
    licenses = validate_licenses()
    digests = validate_digests(packages)
    not_shipped = validate_not_shipped(packages)

    if Enum.empty?(whitelist) && Enum.empty?(licenses) && Enum.empty?(digests) && Enum.empty?(not_shipped) do
      :ok
    else
      %{whitelist: whitelist, licenses: licenses, digests: digests, not_shipped: not_shipped}
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
    |> Enum.map(&:io_lib.format("~2.16.0b", [&1]))
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
    [Application.app_dir(:bom, "priv"), "licenses", to_string(realm), package]
    |> Path.join()
    |> File.read!()
  end

  defp validate_whitelist(packages) do
    Enum.flat_map(@licenses, fn {realm, licenses} ->
      licenses
      |> Enum.map(fn {{name, version}, %{type: license_type}} ->
        %{realm: realm, name: name, license: license_type, version: version}
      end)
      |> Enum.reject(fn candidate ->
        Enum.find(packages, fn package ->
          package.realm == candidate.realm && package.name == candidate.name && package.version == candidate.version
        end)
      end)
    end)
  end

  defp validate_licenses() do
    Enum.flat_map([:node, :elixir, :rust], fn realm ->
      provided =
        [Application.app_dir(:bom, "priv"), "licenses", to_string(realm)]
        |> Path.join()
        |> File.ls!()

      requested =
        @licenses[realm]
        |> Enum.filter(fn {_, %{text: text}} -> text == :provided end)
        |> Enum.map(fn {{name, _}, _} ->
          Path.join([Application.app_dir(:bom, "priv"), "licenses", to_string(realm), name])
        end)

      Enum.map(provided -- requested, &"priv/licenses/#{realm}/#{&1}")
    end)
  end

  defp validate_digests(packages) do
    Map.keys(@type_by_text_digest) --
      (packages
       |> Enum.reject(fn package -> License.allowed_type?(package.license.type) || License.empty?(package.license) end)
       |> Enum.map(&digest(&1.license.text)))
  end

  defp validate_not_shipped(packages) do
    Enum.flat_map(@not_shipped, fn {realm, package_names} ->
      package_names
      |> Enum.reject(fn package_name ->
        Enum.find(packages, fn package ->
          package.realm == realm && package.name == package_name
        end)
      end)
    end)
  end
end
