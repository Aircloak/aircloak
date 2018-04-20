defmodule AirWeb.PrivacyPolicyView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Service.User

  def content_as_html(privacy_policy), do: md_to_safe_html(privacy_policy.content)

  def has_change_info?(privacy_policy), do: not is_nil(privacy_policy.changes)

  def changes_as_html(privacy_policy) do
    if has_change_info?(privacy_policy) do
      md_to_safe_html(privacy_policy.changes)
    else
      nil
    end
  end

  defp has_opted_in?(conn), do: User.privacy_policy_status(conn.assigns.current_user) == :ok

  defp never_accepted_or_rejected?(conn), do: conn.assigns.current_user.accepted_privacy_policy_id == nil

  defp md_to_safe_html(text) do
    text
    |> Earmark.as_html!(%Earmark.Options{gfm: true, breaks: true, smartypants: true})
    |> Aircloak.HTMLScrubber.scrub()
  end
end
