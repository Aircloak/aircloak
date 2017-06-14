defmodule BOM.Gather.Sources do
  @moduledoc "Packes all dependencies into a zip-file."


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Given a list of input directories, this will create a zip file containing them all."
  @spec collect(Keyword.t, String.t) :: :ok
  def collect(dirs, destination_path) do
    tmp_dir_path = create_temp_dir()

    folders_to_zip = Keyword.values(dirs)
    |> collect_deps(tmp_dir_path)
    |> Enum.map(& String.to_charlist/1)

    destination_charlist = String.to_charlist(destination_path)
    base_folder = String.to_charlist(tmp_dir_path)
    :zip.create(destination_charlist, folders_to_zip, [{:compress, :all}, {:cwd, base_folder}])

    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp collect_deps(dirs, destination_path), do:
    Enum.map(dirs, & copy_path(&1, destination_path))

  defp copy_path(dir, destination_dir) do
    target_name = destination_name(dir)
    File.cp_r!(dir, Path.join([destination_dir, target_name]))
    target_name
  end

  # The encryption and base encoding does not serve any security purposes at all.
  # The reason is to generate a unique destination name that doesn't clash between
  # multiple distinct node and elixir imports.
  defp destination_name(dir), do: :crypto.hash(:md4, dir) |> Base.encode16()

  defp create_temp_dir() do
    tmp_dir_path = Path.join([System.tmp_dir!(), "dependencies"])
    File.rm_rf(tmp_dir_path)
    File.mkdir(tmp_dir_path)
    tmp_dir_path
  end
end
