defmodule Air.Admin.GroupView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  def group_name(group) do
    if group.admin do
      [html_escape(group.name), " ", content_tag(:span, "Admin", class: "label label-danger")]
    else
      html_escape(group.name)
    end
  end

  def group_row_class(group) do
    if group.admin, do: "danger"
  end
end
