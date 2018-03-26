defmodule BOM.Gather.Node do
  @moduledoc "Logic for reading node dependency information."

  alias BOM.{Gather, License, Whitelist}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns a list of packages contained in the given `node_modules` directory."
  @spec run(String.t()) :: [Package.t()]
  def run(path) do
    path
    |> list_packages()
    |> Enum.map(&package/1)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp package(path) do
    version = package_json(path, "version")

    %BOM.Package{
      realm: :node,
      name: package_json(path, "name"),
      path: path,
      license: license(path, version),
      version: version
    }
  end

  defp license(path, version) do
    type = license_type(path)

    Gather.public_domain_license(type) || babel_license(path) ||
      Gather.license_from_file(path, type) || Gather.license_from_readme(path, type) ||
      Whitelist.find(:node, Path.basename(path), version)
  end

  defp license_type(path) do
    case package_json(path, "license") do
      nil ->
        package_json(path, "licenses", [])
        |> Enum.map(&package_license_to_type/1)
        |> Enum.find(&License.allowed_type?/1)

      value ->
        package_license_to_type(value)
    end
  end

  defp package_license_to_type(%{"type" => type}), do: package_license_to_type(type)
  defp package_license_to_type(type), do: License.name_to_type(type)

  defp babel_license(path), do: if(babel_package?(path), do: Whitelist.babel_license(), else: nil)

  defp package_json(path, field), do: package_json(path, field, nil)

  defp package_json(path, field, default),
    do:
      Gather.if_matching_file(path, "package.json", fn text -> Poison.decode!(text)[field] end) ||
        default

  @babel_packages ~w(
    babel babel-cli babel-code-frame babel-core babel-generator babel-helper-bindify-decorators
    babel-helper-builder-binary-assignment-operator-visitor
    babel-helper-builder-conditional-assignment-operator-visitor babel-helper-builder-react-jsx
    babel-helper-call-delegate babel-helper-define-map babel-helper-explode-assignable-expression
    babel-helper-explode-class babel-helper-fixtures babel-helper-function-name
    babel-helper-get-function-arity babel-helper-hoist-variables babel-helper-optimise-call-expression
    babel-helper-plugin-test-runner babel-helper-regex babel-helper-remap-async-to-generator
    babel-helper-replace-supers babel-helper-transform-fixture-test-runner babel-helpers babel-messages
    babel-plugin-check-es2015-constants babel-plugin-external-helpers babel-plugin-syntax-async-functions
    babel-plugin-syntax-async-generators babel-plugin-syntax-class-constructor-call
    babel-plugin-syntax-class-properties babel-plugin-syntax-decorators babel-plugin-syntax-do-expressions
    babel-plugin-syntax-exponentiation-operator babel-plugin-syntax-export-extensions babel-plugin-syntax-flow
    babel-plugin-syntax-function-bind babel-plugin-syntax-function-sent babel-plugin-syntax-jsx
    babel-plugin-syntax-object-rest-spread babel-plugin-syntax-trailing-function-commas
    babel-plugin-transform-async-functions babel-plugin-transform-async-to-generator
    babel-plugin-transform-async-to-module-method babel-plugin-transform-class-constructor-call
    babel-plugin-transform-class-properties babel-plugin-transform-decorators
    babel-plugin-transform-do-expressions babel-plugin-transform-es2015-arrow-functions
    babel-plugin-transform-es2015-block-scoped-functions babel-plugin-transform-es2015-block-scoping
    babel-plugin-transform-es2015-classes babel-plugin-transform-es2015-computed-properties
    babel-plugin-transform-es2015-destructuring babel-plugin-transform-es2015-duplicate-keys
    babel-plugin-transform-es2015-for-of babel-plugin-transform-es2015-function-name
    babel-plugin-transform-es2015-instanceof babel-plugin-transform-es2015-literals
    babel-plugin-transform-es2015-modules-amd babel-plugin-transform-es2015-modules-commonjs
    babel-plugin-transform-es2015-modules-systemjs babel-plugin-transform-es2015-modules-umd
    babel-plugin-transform-es2015-object-super babel-plugin-transform-es2015-parameters
    babel-plugin-transform-es2015-shorthand-properties babel-plugin-transform-es2015-spread
    babel-plugin-transform-es2015-sticky-regex babel-plugin-transform-es2015-template-literals
    babel-plugin-transform-es2015-typeof-symbol babel-plugin-transform-es2015-unicode-regex
    babel-plugin-transform-es3-member-expression-literals babel-plugin-transform-es3-property-literals
    babel-plugin-transform-es5-property-mutators babel-plugin-transform-eval
    babel-plugin-transform-exponentiation-operator babel-plugin-transform-export-extensions
    babel-plugin-transform-flow-comments babel-plugin-transform-flow-strip-types
    babel-plugin-transform-function-bind babel-plugin-transform-jscript babel-plugin-transform-object-assign
    babel-plugin-transform-object-rest-spread babel-plugin-transform-object-set-prototype-of-to-assign
    babel-plugin-transform-proto-to-assign babel-plugin-transform-react-constant-elements
    babel-plugin-transform-react-display-name babel-plugin-transform-react-inline-elements
    babel-plugin-transform-react-jsx babel-plugin-transform-react-jsx-compat
    babel-plugin-transform-react-jsx-self babel-plugin-transform-react-jsx-source
    babel-plugin-transform-regenerator babel-plugin-transform-runtime babel-plugin-transform-strict-mode
    babel-plugin-undeclared-variables-check babel-polyfill babel-preset-es2015 babel-preset-es2016
    babel-preset-es2017 babel-preset-latest babel-preset-react babel-preset-stage-0 babel-preset-stage-1
    babel-preset-stage-2 babel-preset-stage-3 babel-register babel-runtime babel-template babel-traverse
    babel-types
  )
  defp babel_package?(path) do
    Enum.member?(@babel_packages, Path.basename(path))
  end

  defp list_packages(path) do
    sub_paths =
      path
      |> Path.join("*")
      |> Path.wildcard()

    sub_paths
    |> Enum.all?(&File.dir?/1)
    |> case do
      true ->
        # This is a normal node_modules folder with sub-packages
        Enum.flat_map(sub_paths, fn package_path ->
          [
            package_path
            | package_path |> Path.join("node_modules") |> list_packages()
          ]
        end)

      false ->
        # This seems to be a folder which contains a package embedded directly
        # inside of it. `clean-pslg` is an example. We here have to treat the
        # directory as the top-level directory for a package, rather than a
        # parent directory to multiple packages.
        [path]
    end
  end
end
