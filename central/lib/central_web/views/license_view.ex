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
    %{
      "LDAP" => "ldap",
      "Offline deployment" => "offline_deployment",
      "Analyst tables" => "analyst_tables",
      "Email support" => "email_support"
    }
  end

  defp datasource_features() do
    %{
      "Postgres" => "ds_postgres",
      "MySQL and MariaDB" => "ds_mysql",
      "SQL Server" => "ds_sqlserver",
      "MongoDB" => "ds_mongodb",
      "Apache Drill" => "ds_apachedrill",
      "Oracle DB" => "ds_oracle"
    }
  end

  defp name_features(mapping, license),
    do:
      mapping
      |> Enum.filter(fn {name, key} -> Enum.member?(license.features, key) end)
      |> Enum.map(fn {name, _} -> name end)

  defp datasource_features(license), do: name_features(datasource_features(), license)

  defp core_features(license), do: name_features(features(), license)

  defp licensed_features(license), do: Aircloak.OxfordComma.join(core_features(license) ++ datasource_features(license))
end
