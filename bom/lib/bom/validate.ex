defmodule BOM.Validate do
  @moduledoc "Contains checks to be performed on packages, to ensure they can be used in our product."

  alias BOM.License


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Performs all checks. Sets the error field if any of them fail."
  @spec run(Package.t) :: Package.t
  def run(package) do
    %{package | error: errors(package)}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp errors(%{license: nil}), do: "No license found"
  defp errors(%{license: license}) do
    license_text_error(license) || license_type_error(license)
  end
  defp errors(_), do: nil

  defp license_type_error(%License{type: type}) do
    if License.allowed_type?(type), do: nil, else: "Forbidden license type - #{inspect(type)}"
  end

  defp license_text_error(license) do
    text = (license.text || "") |> String.trim()
    if text == "", do: "License empty", else: nil
  end
end
