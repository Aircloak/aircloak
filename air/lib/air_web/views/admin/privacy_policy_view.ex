defmodule AirWeb.Admin.PrivacyPolicyView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.PrivacyPolicy

  defp format_datetime(datetime), do: Timex.format!(datetime, "{YYYY}-{0M}-{0D} {0h24}:{0m}")

  defp first_policy?(), do: not PrivacyPolicy.exists?()

  defp content() do
    case PrivacyPolicy.get() do
      {:error, :no_privacy_policy_created} ->
        Earmark.as_html!("You have not defined a privacy policy yet.")

      {:ok, privacy_policy} ->
        Earmark.as_html!(privacy_policy.content)
    end
  end
end
