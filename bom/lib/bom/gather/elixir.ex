defmodule BOM.Gather.Elixir do
  @moduledoc "Logic for reading elixir dependency information."

  alias BOM.{Gather,License}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns a list of packages contained in the given `deps` directory."
  @spec run(String.t) :: [Package.t]
  def run(deps_path) do
    deps_path
    |> version_map()
    |> Enum.map(&package(&1, deps_path))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp package({package, version}, deps_path) do
    package_path = Path.join(deps_path, package)

    %BOM.Package{
      realm: :elixir,
      name: package,
      license: license(package_path),
      version: version
    }
  end

  defp license(path) do
    type = license_type(path)

    Gather.public_domain_license(type) ||
      Gather.license_from_file(path, type) ||
      Gather.license_from_readme(path, type) ||
      BOM.Whitelist.find(:elixir, package_name(path))
  end

  defp license_type(path) do
    case path |> package_name() |> BOM.Gather.Elixir.Hex.package() do
      {:error, _} -> nil
      {:ok, %{"meta" => %{"licenses" => nil}}} -> nil
      {:ok, %{"meta" => %{"licenses" => licenses}}} ->
        licenses
        |> Enum.map(&License.name_to_type/1)
        |> Enum.find(&License.allowed_type?/1)
    end
  end

  defp version_map(deps_path) do
    Gather.if_matching_file(deps_path, "../mix.lock", fn text ->
      {deps, []} = Code.eval_string(text)

      for {package, spec} <- deps, into: %{} do
        [_, _, version | _] = Tuple.to_list(spec)
        {to_string(package), version}
      end
    end)
  end

  defp package_name(path), do: Path.basename(path)
end
