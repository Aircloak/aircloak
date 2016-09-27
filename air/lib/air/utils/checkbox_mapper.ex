defmodule Air.Utils.CheckboxMapper do
  @moduledoc """
  Mappers for the PhoneixMTM checkbox MTM mapping library
  """
  use PhoenixMTM.Mappers

  alias Air.Group


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def for_users(form, field, input_opts, group, label_opts, _opts) do
    content_tag(:div, class: "checkbox") do
      label(form, field, label_opts) do
        [
          tag(:input, input_opts),
          group_label(group)
        ]
      end
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp group_label(%Group{admin: false, name: name}), do: html_escape(name)
  defp group_label(%Group{admin: true, name: name}) do
    [
      html_escape(name), raw("&nbsp;"),
      content_tag(:span, class: "label label-danger") do
        "Admin"
      end
    ]
  end
end

