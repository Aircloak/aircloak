defmodule AirWeb.Admin.UserView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Repo

  defp checkbox_mapper(form, field, input_opts, group, label_opts, _opts) do
    content_tag(:div, class: "checkbox") do
      label(form, field, label_opts) do
        [
          tag(:input, input_opts),
          Air.Utils.CheckboxMapper.group_label_text(group),
          content_tag(:small, class: "newline") do
            data_sources_given_access_to(group)
          end
        ]
      end
    end
  end

  @num_to_take 5
  defp data_sources_given_access_to(group) do
    group = Repo.preload(group, :data_sources)

    group.data_sources
    |> Enum.map(& &1.name)
    |> case do
      [] ->
        ["Doesn't give access to any data sources"]

      names when length(names) <= @num_to_take ->
        [
          raw("Gives access to #{length(names)} data_sources: ")
          | Air.Utils.CheckboxMapper.highlighted_and_comma_separated(names, @num_to_take)
        ]

      names ->
        [
          raw("Gives access to #{length(names)} data_sources, including: ")
          | Air.Utils.CheckboxMapper.highlighted_and_comma_separated(names, @num_to_take)
        ]
    end
  end

  defp login(user) do
    Air.Service.User.main_login(user)
  end

  defp can_disable?(conn, user) do
    cond do
      is_self?(conn, user) -> false
      true -> can_disable?(user)
    end
  end

  defp can_disable?(%{source: :ldap}), do: false
  defp can_disable?(%{source: :native, enabled: enabled}), do: enabled

  defp can_enable?(%{source: :ldap}), do: false
  defp can_enable?(%{source: :native, enabled: enabled}), do: not enabled

  defp can_delete?(conn, user) do
    cond do
      is_self?(conn, user) -> false
      true -> can_delete?(user)
    end
  end

  defp can_delete?(%{source: :ldap, enabled: enabled}), do: not enabled
  defp can_delete?(%{source: :native}), do: true

  defp can_edit?(%{source: :ldap}), do: false
  defp can_edit?(%{source: :native}), do: true

  defp is_self?(conn, user), do: conn.assigns.current_user.id == user.id
end
