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
      "[Jira] email support" => "email_support"
    }
  end

  defp datasource_features() do
    %{
      "Postgres" => "ds_postgres",
      "SQL Server" => "ds_sqlserver",
      "MongoDB" => "ds_mongodb",
      "Oracle DB" => "ds_oracle",
      "Cloudera Impala" => "ds_impala"
    }
  end

  defp includes_all?(license, features) do
    features
    |> Enum.map(fn {_name, key} -> key end)
    |> Enum.all?(&Enum.member?(license.features, &1))
  end

  defp name_features(license) do
    available_features = Map.merge(features(), datasource_features())

    feature_to_name = fn features ->
      features
      |> Enum.filter(fn {_name, key} -> Enum.member?(license.features, key) end)
      |> Enum.map(fn {name, _} -> name end)
    end

    if includes_all?(license, available_features) do
      ["All licensensable features"]
    else
      core_features =
        if includes_all?(license, features()) do
          ["All core features"]
        else
          feature_to_name.(features())
        end

      data_sources =
        if includes_all?(license, datasource_features()) do
          ["All data sources"]
        else
          feature_to_name.(datasource_features())
        end

      core_features ++ data_sources
    end
  end

  defp licensed_features(license), do: license |> name_features() |> Aircloak.OxfordComma.join()
end
