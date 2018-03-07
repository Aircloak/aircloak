defmodule CentralWeb.LicenseView do
  @moduledoc false
  use Central.Web, :view

  defp expires(%{auto_renew: true}), do: ""
  defp expires(license) do
    expiry = Central.Service.License.expires_at(license)
    "#{Timex.format!(expiry, "{ISOdate} {h24}:{m}:{s}")} #{Timex.format!(expiry, "({relative})", :relative)}"
  end
end
