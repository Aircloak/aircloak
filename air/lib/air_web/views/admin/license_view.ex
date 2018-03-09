defmodule AirWeb.Admin.LicenseView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.License

  def license_valid?(), do: License.valid?()

  def license_expiry(), do: Liense.expiry()
end
