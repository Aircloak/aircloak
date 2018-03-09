defmodule AirWeb.Admin.LicenseView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.License

  defp license_valid?(), do: License.valid?()

  defp license_expiry(), do: License.expiry()

  defp license_present?(), do: License.present?()
end
