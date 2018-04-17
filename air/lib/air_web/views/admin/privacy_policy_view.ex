defmodule AirWeb.Admin.PrivacyPolicyView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.PrivacyPolicy

  defp format_datetime(datetime), do: Timex.format!(datetime, "{YYYY}-{0M}-{0D} {0h24}:{0m}")

  defp text_content(nil), do: PrivacyPolicy.default_content()
  defp text_content(privacy_policy), do: privacy_policy.content

  defp first_privacy_policy?(nil), do: true
  defp first_privacy_policy?(_), do: false

  defp rendered_content(nil), do: Earmark.as_html!("You have not defined a privacy policy yet.")
  defp rendered_content(privacy_policy), do: Earmark.as_html!(privacy_policy.content)
end
