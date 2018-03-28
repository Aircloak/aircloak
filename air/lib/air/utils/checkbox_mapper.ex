defmodule Air.Utils.CheckboxMapper do
  @moduledoc """
  Utilities used when creating checkboxes using the PhoenixMTM library
  """
  alias Air.Schemas.Group

  import Phoenix.HTML.Tag, only: [content_tag: 2, content_tag: 3]
  import Phoenix.HTML, only: [html_escape: 1, raw: 1]

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Given a group model, it will produce a HTML safe representation of the
  strongly formatted group name, along with auxiliary labels indication
  properties of the group, such as whether it gives admin privileges.
  """
  @spec group_label_text(Group.t()) :: [Phoenix.HTML.safe()]
  def group_label_text(%Group{admin: false, name: name}), do: content_tag(:strong, html_escape(name))

  def group_label_text(%Group{admin: true, name: name}) do
    [
      content_tag(:strong, html_escape(name)),
      raw("&nbsp;"),
      content_tag(:span, class: "label label-danger") do
        "Admin"
      end
    ]
  end

  @doc """
  Takes a list of entities, and returns N of them, strongly formatted,
  and intersperced with commas.
  """
  @spec highlighted_and_comma_separated([Phoenix.HTML.unsafe()], integer) :: [Phoenix.HTML.safe()]
  def highlighted_and_comma_separated(entities, n) do
    entities
    |> Enum.take(n)
    |> Enum.map(&content_tag(:strong, html_escape(&1)))
    |> Enum.intersperse(raw(", "))
  end
end
