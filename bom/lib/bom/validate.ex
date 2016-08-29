defmodule BOM.Validate do
  alias BOM.License

  def run(package) do
    %{package | error: errors(package)}
  end

  def errors(%{license: nil}), do: "No license found"
  def errors(%{license: license}) do
    license_text_error(license) || license_type_error(license)
  end
  def errors(_), do: nil

  defp license_type_error(%License{type: type}) do
    if License.allowed_type?(type), do: nil, else: "Forbidden license type - #{inspect(type)}"
  end

  defp license_text_error(license) do
    text = (license.text || "") |> String.trim()
    if text == "", do: "License empty", else: nil
  end
end
