defmodule AirWeb.SharedView do
  @moduledoc false
  use Air.Web, :view

  def version(), do: Aircloak.Version.for_app(:air)

  def has_privacy_policy?(), do: Air.Service.PrivacyPolicy.exists?()

  def licensed_feature?(feature_name), do: Enum.member?(Air.Service.License.features(), feature_name)
end
