defmodule AirWeb.PrivacyPolicyView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.{PrivacyPolicy, User}

  defp content_as_html() do
    case PrivacyPolicy.get() do
      {:error, :no_privacy_policy_created} ->
        Earmark.as_html!("""
          The privacy policy is currently missing.
          Please ask your administrator to define it in the admin section.
        """)

      {:ok, privacy_policy} ->
        Earmark.as_html!(privacy_policy.content)
    end
  end

  defp has_change_info?() do
    case PrivacyPolicy.get() do
      {:error, :no_privacy_policy_created} -> false
      {:ok, privacy_policy} -> privacy_policy.changes != ""
    end
  end

  defp changes_as_html() do
    case PrivacyPolicy.get() do
      {:error, :no_privacy_policy_created} -> ""
      {:ok, privacy_policy} -> Earmark.as_html!(privacy_policy.changes)
    end
  end

  defp has_opted_in?(conn), do: User.privacy_policy_status(conn.assigns.current_user) == :ok

  defp never_accepted_or_rejected?(conn), do: conn.assigns.current_user.accepted_privacy_policy_id == nil
end
