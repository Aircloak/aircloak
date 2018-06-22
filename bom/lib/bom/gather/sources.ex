defmodule BOM.Gather.Sources do
  @moduledoc "Bundles all dependencies into a zip-file."

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Given a list of input directories, this will create a zip file containing them all."
  @spec collect(Keyword.t(), String.t()) :: :ok
  def collect(dirs, destination_path) do
    tmp_dir_path = BOM.Util.create_temp_dir("dependencies")

    dirs
    |> collect_deps(tmp_dir_path)
    |> create_zip_archive(destination_path, tmp_dir_path)

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp create_zip_archive(folders_to_zip, destination_path, tmp_dir_path) do
    folders_to_zip = Enum.map(folders_to_zip, &String.to_charlist/1)
    destination_charlist = String.to_charlist(destination_path)
    base_folder = String.to_charlist(tmp_dir_path)
    :zip.create(destination_charlist, folders_to_zip, [{:compress, :all}, {:cwd, base_folder}])
  end

  defp collect_deps(dirs, destination_path), do: Enum.map(dirs, &copy_path(&1, destination_path))

  defp copy_path(dir, destination_dir) do
    target_name = destination_name(dir)
    target_path = Path.join([destination_dir, target_name])
    File.mkdir_p!(target_path)
    File.cp_r!(dir, target_path)
    target_name
  end

  # Constructs a destination name based on the path given.
  # It is assumed that the path takes the form: <some path ...>/<project>/<dep-folder>.
  # The latter two components will be used as a the name.
  # Hence the `air` node-js dependenceis will end up as: `air/node_modules`.
  # If the project and dependency type can't be derived from the file name,
  # a random hash value is used instead, to provide a unique name for the folder.
  defp destination_name(dir) do
    case Path.split(dir) |> Enum.take(-2) do
      [_project, _dep_folder] = segments -> Path.join(segments)
      _other -> :crypto.strong_rand_bytes(6) |> Base.encode16()
    end
  end
end
