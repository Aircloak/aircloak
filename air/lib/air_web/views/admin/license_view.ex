defmodule AirWeb.Admin.LicenseView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.{License, Warnings}

  defp license_valid?(), do: License.valid?()

  defp license_expiry(), do: License.expiry() |> Timex.format!("{ISOdate}")

  defp license_present?(), do: License.present?()

  defp license_auto_renews?(), do: License.auto_renew?()

  defp conditional_positive_glyph_if_feature(feature_name) do
    if AirWeb.SharedView.licensed_feature?(feature_name) do
      Phoenix.HTML.raw(
        "<span class='glyphicon glyphicon glyphicon-ok-circle' style='color: green' aria-hidden='true'></span>"
      )
    end
  end

  defp license_warning() do
    case Warnings.problems_for_resource(:license) do
      [warning | _] -> warning.description
      _ -> nil
    end
  end
end
