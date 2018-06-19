defmodule BOM.Gather.Rust do
  @moduledoc "Logic for reading rust dependency information."

  alias BOM.{Package, Whitelist}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns a list of packages that the rust project in the given directory depends on."
  @spec run(String.t()) :: [Package.t()]
  def run(path) do
    path
    |> load_cargo_lock()
    |> extract_dependencies()
    |> Enum.map(&normalize/1)
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

  defp normalize(%{name: name, version: version}) do
    %Package{realm: :rust, name: name, version: version, license: Whitelist.find(:rust, name, version)}
  end
end
