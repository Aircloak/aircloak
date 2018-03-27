defmodule BOM.Gather do
  @moduledoc "Common functions used for gathering package information."

  alias BOM.License

  @license_files "*{LICENSE,LICENCE,license,licence,License,Licence}*"
  @readme_files "*{README,Readme,readme}*"

  @doc """
  Returns a License if a commonly named license file can be found in the given directory. The license will
  have the given type and its text will be taken from the file. Returns nil if such a file cannot be found.
  """
  @spec license_from_file(Path.t(), atom) :: License.t() | nil
  def license_from_file(path, license_type),
    do:
      if_matching_file(path, @license_files, fn text ->
        %License{type: license_type, text: text}
      end)

  @doc """
  Returns a License if a commonly named readme file can be found in the given directory and a license section
  can automatically be found within. The license will have the given type and its text will be taken from the
  file. Returns nil if such a file cannot be found.
  """
  @spec license_from_readme(Path.t(), atom) :: License.t() | nil
  def license_from_readme(path, license_type) do
    if_matching_file(path, @readme_files, fn text ->
      case Regex.run(~r/\n#* ?(license|licence)(.|\n)*/i, text) do
        [text | _] -> %License{type: license_type, text: text}
        _ -> nil
      end
    end)
  end

  @doc "Returns a License representing a public domain license if the type is `:public_domain`, nil otherwise."
  @spec public_domain_license(atom) :: License.t() | nil
  def public_domain_license(:public_domain), do: License.find_by_type(:public_domain)
  def public_domain_license(_type), do: nil

  @doc """
  Calls `action` with the text of a file under `path` matching the glob `pattern`, returning its result.
  Returns nil if such a file cannot be found or opened.
  """
  @spec if_matching_file(Path.t(), String.t(), (String.t() -> x)) :: x | nil when x: var
  def if_matching_file(path, pattern, action) do
    path
    |> Path.join(pattern)
    |> Path.wildcard()
    |> Enum.find(&File.exists?/1)
    |> case do
      nil ->
        nil

      matching_path ->
        File.read(matching_path)
        |> case do
          {:ok, text} -> action.(text)
          _ -> nil
        end
    end
  end
end
