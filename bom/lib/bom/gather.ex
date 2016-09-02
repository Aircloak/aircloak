defmodule BOM.Gather do
  alias BOM.License

  @license_files "*{LICENSE,LICENCE,license,licence,License,License}*"
  @readme_files "*{README,Readme,readme}*"

  def license_from_file(path, license_type), do:
    if_matching_file(path, @license_files, fn(text) -> %License{type: license_type, text: text} end)

  def license_from_readme(path, license_type) do
    if_matching_file(path, @readme_files, fn text ->
      case Regex.run(~r/\n#* ?(license|licence)(.|\n)*/i, text) do
        [text | _] -> %License{type: license_type, text: text}
        _ -> nil
      end
    end)
  end

  def public_domain_license(:public_domain), do: License.find_by_type(:public_domain)
  def public_domain_license(_), do: nil

  def if_matching_file(path, pattern, action) do
    path
    |> Path.join(pattern)
    |> Path.wildcard()
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> nil
      matching_path ->
        File.read(matching_path)
        |> case do
          {:ok, text} -> action.(text)
          _ -> nil
        end
    end
  end
end
