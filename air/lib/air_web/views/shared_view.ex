defmodule AirWeb.SharedView do
  @moduledoc false
  use Air.Web, :view

  def version(), do: Aircloak.Version.for_app(:air)

  def has_privacy_policy?(), do: Air.Service.PrivacyPolicy.exists?()
end
