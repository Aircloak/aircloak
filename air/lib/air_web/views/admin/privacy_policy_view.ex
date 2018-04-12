defmodule AirWeb.Admin.PrivacyPolicyView do
  @moduledoc false
  use Air.Web, :view

  defp format_datetime(datetime), do: Timex.format!(datetime, "{YYYY}-{0M}-{0D} {0h24}:{0m}")

  defp first_policy?(), do: not Air.Service.PrivacyPolicy.exists?()
end
