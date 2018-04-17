defmodule AirWeb.PrivacyPolicyView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.User

  defp content_as_html(privacy_policy), do: Earmark.as_html!(privacy_policy.content)

  defp has_change_info?(privacy_policy), do: not is_nil(privacy_policy.changes)

  defp changes_as_html(privacy_policy) do
    if has_change_info?(privacy_policy) do
      Earmark.as_html!(privacy_policy.changes)
    else
      nil
    end
  end

  defp has_opted_in?(conn), do: User.privacy_policy_status(conn.assigns.current_user) == :ok

  defp never_accepted_or_rejected?(conn), do: conn.assigns.current_user.accepted_privacy_policy_id == nil
end
