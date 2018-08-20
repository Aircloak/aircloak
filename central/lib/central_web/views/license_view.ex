defmodule CentralWeb.LicenseView do
  @moduledoc false
  use Central.Web, :view

  defp expires(%{auto_renew: true}), do: ""

  defp expires(license) do
    expiry = Central.Service.License.expires_at(license)

    "#{Timex.format!(expiry, "{ISOdate} {h24}:{m}:{s}")} #{Timex.format!(expiry, "({relative})", :relative)}"
  end

  defp revoke_class(%{revoked: true}), do: "danger"
  defp revoke_class(_), do: ""

  defp feature_enabled?(%{data: %{features: nil}}, _), do: false
  defp feature_enabled?(%{data: %{features: features}}, feature), do: feature in features

  defp features() do
    %{"LDAP" => "ldap"}
  end
end
