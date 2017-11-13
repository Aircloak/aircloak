defmodule BOM.Gather.Elixir do
  @moduledoc "Logic for reading elixir dependency information."

  alias BOM.{Gather,License}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns a list of packages contained in the given `deps` directory."
  @spec run(String.t) :: [Package.t]
  def run(deps_path) do
    version_map = version_map(deps_path)

    deps_path
    |> Path.join(otp_version())
    |> Path.join(elixir_version())
    |> Path.join("*")
    |> Path.wildcard()
    |> Enum.map(&{&1, version_map[package_name(&1)]})
    |> Enum.filter(fn {_, version} -> version end)
    |> Enum.map(&package/1)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp package({path, version}) do
    %BOM.Package{
      realm: :elixir,
      name: package_name(path),
      path: path,
      license: license(path, version),
      version: version,
    }
  end

  defp license(path, version) do
    type = license_type(path)

    Gather.public_domain_license(type) ||
      Gather.license_from_file(path, type) ||
      Gather.license_from_readme(path, type) ||
      BOM.Whitelist.find(:elixir, package_name(path), version)
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

  defp otp_version(), do:
    [:code.root_dir(), "releases", :erlang.system_info(:otp_release), "OTP_VERSION"]
    |> Path.join()
    |> File.read!()
    |> String.trim("\n")

  defp elixir_version(), do:
    System.version()
end
