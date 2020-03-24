defmodule AirWeb.Admin.LicenseView do
  @moduledoc false
  use Air.Web, :view
  import Phoenix.HTML.Tag

  alias Air.Service.{License, Warnings}

  defp license_valid?(), do: License.valid?()

  defp license_expiry(), do: License.expiry() |> Timex.format!("{ISOdate}")

  defp license_present?(), do: License.present?()

  defp license_auto_renews?(), do: License.auto_renew?()

  def feature_row(label, feature_name) when not is_boolean(feature_name),
    do: feature_row(label, AirWeb.SharedView.licensed_feature?(feature_name))

  def feature_row(label, enabled) do
    content_tag(:tr, [
      content_tag(:td, label, class: if(enabled, do: '', else: 'text-muted')),
      content_tag(
        :td,
        if(enabled,
          do: [
            tag(:i, class: "fas fa-check-circle text-success", "aria-hidden": true),
            content_tag(:span, "Enabled", class: "sr-only")
          ]
        )
      )
    ])
  end

  defp license_warning() do
    case Warnings.problems_for_resource(:license) do
      [warning | _] -> warning.description
      _ -> nil
    end
  end
end
