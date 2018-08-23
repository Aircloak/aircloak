defmodule AirWeb.SharedView do
  @moduledoc false
  use Air.Web, :view

  def version() do
    :air
    |> Aircloak.Version.for_app()
    |> Aircloak.Version.to_string()
  end

  def has_privacy_policy?(), do: Air.Service.PrivacyPolicy.exists?()
end
