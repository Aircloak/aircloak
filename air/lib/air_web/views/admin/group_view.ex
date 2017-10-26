defmodule AirWeb.Admin.GroupView do
  @moduledoc false
  use Air.Web, :view

  alias Air.{Repo, Schemas.Group}

  def group_name(group) do
    if group.admin do
      [html_escape(group.name), " ", content_tag(:span, "Admin", class: "label label-danger")]
    else
      html_escape(group.name)
    end
  end

  def group_row_class(group), do: if group.admin, do: "danger"

  def names(groups) do
    groups
    |> Enum.map(&(&1.name))
    |> Enum.map(&shorten_name/1)
    |> format_names()
  end

  def format_names([]), do: "No groups"
  def format_names([group]), do: group
  def format_names([group1, group2]), do: "#{group1}, #{group2}"
  def format_names([group1, group2, group3]), do: "#{group1}, #{group2}, and #{group3}"
  def format_names(groups) do
    {first_groups, rest} = Enum.split(groups, 2)
    first_groups ++ ["and #{length(rest)} other groups"]
    |> Enum.join(", ")
  end

  @max_length 10
  @front_length trunc(Float.ceil(2 * @max_length / 3)) - 1
  @end_length trunc(Float.floor(@max_length / 3)) - 1

  def shorten_name(name) do
    if String.length(name) > @max_length do
      {first, _} = String.split_at(name, @front_length)
      first = String.trim(first)
      {_, last} = String.split_at(name, String.length(name) - @end_length)
      last = String.trim(last)
      "#{first}..#{last}"
    else
      name
    end
  end

  def available_groups() do
    Repo.all(Group)
  end

  defp checkbox_mapper(form, field, input_opts, {name, description}, label_opts, _opts) do
    content_tag(:tr) do
      [
        content_tag(:td, class: "col-md-1") do
          tag(:input, input_opts)
        end,
        content_tag(:td, class: "col-md-11") do
          label(form, field, label_opts) do
            if description do
              [
                name,
                content_tag(:small, class: "newline") do
                  description
                end
              ]
            else
              name
            end
          end
        end
      ]
    end
  end
end
