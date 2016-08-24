defmodule BOM.Validate do
  def run(package) do
    %{package | error: errors(package)}
  end

  def errors(%{license: nil}), do: "No license found"
  def errors(%{license: license}) do
    license_text_error(license) || license_type_erro(license)
  end
  def errors(_), do: nil

  defp license_type_erro(license) do
    if !BOM.License.allowed_type?(license), do: "Forbidden license type - #{license.type}", else: nil
  end

  defp license_text_error(license) do
    text = (license.text || "") |> String.trim()
    if text == "", do: "License empty", else: nil
  end
end
