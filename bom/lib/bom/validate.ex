defmodule BOM.Validate do
  @moduledoc "Contains checks to be performed on packages, to ensure they can be used in our product."

  alias BOM.License

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Performs all checks. Sets the error field if any of them fail."
  @spec run(Package.t()) :: Package.t()
  def run(package) do
    %{package | error: errors(package)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp errors(%{name: nil}), do: "Name empty"
  defp errors(%{name: ""}), do: "Name empty"
  defp errors(%{version: nil}), do: "Version empty"
  defp errors(%{version: ""}), do: "Version empty"
  defp errors(%{realm: nil}), do: "Realm empty"
  defp errors(%{license: nil}), do: "No license"
  defp errors(%{license: license}), do: license_type_error(license) || license_text_error(license)
  defp errors(_), do: nil

  defp license_type_error(%License{type: type}),
    do:
      if(License.allowed_type?(type), do: nil, else: "Forbidden license type - #{inspect(type)}")

  defp license_text_error(license),
    do: if(License.empty?(license), do: "License empty", else: nil)
end
