defmodule AirWeb.Admin.LicenseView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.{License, Warnings}

  defp license_valid?(), do: License.valid?()

  defp license_expiry(), do: License.expiry() |> Timex.format!("{ISOdate}")

  defp license_present?(), do: License.present?()

  defp license_warning() do
    case Warnings.problems_for_resource(:license) do
      [warning | _] -> warning.description
      _ -> nil
    end
  end
end
