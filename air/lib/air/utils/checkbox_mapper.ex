defmodule Air.Utils.CheckboxMapper do
  @moduledoc """
  Mappers for the PhoneixMTM checkbox MTM mapping library
  """
  use PhoenixMTM.Mappers

  alias Air.{Group, Repo}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def for_users(form, field, input_opts, group, label_opts, _opts) do
    content_tag(:div, class: "checkbox") do
      label(form, field, label_opts) do
        [
          tag(:input, input_opts),
          group_label_text(group)
        ]
      end
    end
  end

  def for_data_source(form, field, input_opts, group, label_opts, _opts) do
    content_tag(:div, class: "checkbox") do
      label(form, field, label_opts) do
        [
          tag(:input, input_opts),
          users_label_text(group)
        ]
      end
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp group_label_text(%Group{admin: false, name: name}), do: content_tag(:strong, html_escape(name))
  defp group_label_text(%Group{admin: true, name: name}) do
    [
      content_tag(:strong, html_escape(name)), raw("&nbsp;"),
      content_tag(:span, class: "label label-danger") do
        "Admin"
      end
    ]
  end

  defp users_label_text(group) do
    [
      group_label_text(group),
      raw(" &ndash; ") | users_given_access_to(group)
    ]
  end

  @num_to_take 5

  defp users_given_access_to(group) do
    group = Repo.preload(group, :users)
    case group.users do
      [] -> ["Doesn't give access to any users"]
      users when length(users) <= @num_to_take ->
        [raw("Gives access to #{length(users)} users: ") | user_names(users)]
      users ->
        [raw("Gives access to #{length(users)} users, including: ") | user_names(users)]
    end
  end

  defp user_names(users) do
    users
    |> Enum.take(@num_to_take)
    |> Enum.map(&content_tag(:strong, html_escape(&1.name)))
    |> Enum.intersperse(raw(", "))
  end
end

