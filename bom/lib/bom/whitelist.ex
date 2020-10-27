defmodule BOM.Whitelist do
  @moduledoc "Contains information about packages for which a license or its type could not be automatically found."

  alias BOM.{License, Package}

  # -------------------------------------------------------------------
  # Whitelists
  # -------------------------------------------------------------------

  @licenses %{
    :elixir => %{
      {"ecto_enum", "1.4.0"} => %{type: :mit, text: :provided},
      {"excoveralls", "0.5.7"} => %{type: :mit, text: :standard},
      {"file_system", "0.2.8"} => %{type: :wtfpl, text: :standard},
      {"makeup_elixir", "0.14.1"} => %{type: :bsd_4_clause, text: :standard},
      {"scrivener_ecto", "2.2.0"} => %{type: :mit, text: :provided},
      {"scrivener", "2.7.0"} => %{type: :mit, text: :provided}
    },
    :node => %{
      {"spdx-exceptions", "2.3.0"} => %{type: :cca3u, text: :standard}
    },
    :rust => %{
      {"cfg-if", "0.1.5"} => %{type: :mit, text: :provided},
      {"log", "0.4.5"} => %{type: :mit, text: :provided},
      {"odbc-safe", "0.5.0"} => %{type: :mit, text: :provided},
      {"odbc-sys", "0.8.2"} => %{type: :mit, text: :provided},
      {"odbc", "0.17.0"} => %{type: :mit, text: :provided},
      {"doc-comment", "0.3.3"} => %{type: :mit, text: :standard},
      {"encoding_rs", "0.8.24"} => %{type: :mit, text: :standard},
      {"simple-error", "0.1.11"} => %{type: :mit, text: :provided}
    }
  }

  @type_by_text_digest %{
    # node/type-fest
    "915042b5df33c31a6db2b37eadaa00e3" => :mit,
    # node/pako
    "a4f08d6b2d1bf3f3a1bc296a6109a25b" => :mit,
    # node/caniuse-lite
    "60f8103054954b2c75f1faa295ea3590" => :cca4i,
    # node/atob
    "493adefc1fe80b079a23203e4732d945" => :mit,
    # node/phoenix_html
    "1dc701356996e3d0dd135248577c8ef7" => :mit,
    # elixir/earmark
    "1bf8028080e75e094cd7b53003c2efeb" => :apache2,
    # node/mousetrap
    "573d6504860bb20f6f58dc76f7778c85" => :apache2,
    # node/node-forge
    "3468e584d830bfb0ffd2d0af6e129136" => :bsd_3_clause,
    # node/sha.js
    "71f5a3fe755d4bb9cb62b97bdad36e45" => :mit,
    # node/tslib
    "f938d99cba29007eeae26d80a9a4cfa6" => :bsd_0_clause,
    # node/dompurify
    "0c419ae0ef32e42c7f620d5d2cac30aa" => :apache2
  }

  @not_shipped %{
    elixir: ~w(proper rustler),
    node: ~w(
      eslint eslint-plugin-import eslint-plugin-jsx-a11y axe-core language-subtag-registry eslint-plugin-react eslint-import-resolver-node
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
          name
        end)

      Enum.map((provided -- requested) -- ["babel"], &"priv/licenses/#{realm}/#{&1}")
    end)
    |> Enum.sort()
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
