defmodule BOM.Gather.Rust do
  @moduledoc "Logic for reading rust dependency information."

  alias BOM.{Util, Package, Whitelist}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns a path to downloaded sources and a list of packages that the rust project in the given directory depends on.
  """
  @spec run(String.t()) :: {String.t(), [Package.t()]}
  def run(path) do
    temp_dir = Util.create_temp_dir("rust")

    packages =
      path
      |> load_cargo_lock()
      |> extract_dependencies()

    Enum.each(packages, &download_source(&1, temp_dir))

    {temp_dir, Enum.map(packages, &normalize/1)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_cargo_lock(path) do
    path
    |> Path.join("Cargo.lock")
    |> File.read!()
    |> Tomlex.load()
  end

  defp extract_dependencies(cargo_lock) do
    Enum.filter(cargo_lock[:package], & &1[:source])
  end

  defp download_source(%{name: name, version: version}, temp_dir) do
    {200, result} = Util.https_get("https://crates.io/api/v1/crates/#{name}/#{version}/download")

    File.write!(Path.join(temp_dir, "#{name}-#{version}.tar.gz"), result)
  end

  defp normalize(%{name: name, version: version}) do
    %Package{realm: :rust, name: name, version: version, license: Whitelist.find(:rust, name, version)}
  end
end
