defmodule AirWeb.Admin.DataSourceView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Repo

  defdelegate availability_label(data_source), to: AirWeb.DataSourceView
  defdelegate number_of_tables(data_source), to: AirWeb.DataSourceView

  def available?(data_source), do: Air.Service.DataSource.available?(data_source.name)

  defp checkbox_mapper(form, field, input_opts, group, label_opts, _opts) do
    content_tag(:div, class: "checkbox") do
      label(form, field, label_opts) do
        [
          tag(:input, input_opts),
          Air.Utils.CheckboxMapper.group_label_text(group),
          raw(" &ndash; ") | users_given_access_to(group)
        ]
      end
    end
  end

  @num_to_take 5
  defp users_given_access_to(group) do
    group = Repo.preload(group, :users)

    group.users
    |> Enum.map(& &1.name)
    |> case do
      [] ->
        ["Doesn't give access to any users"]

      names when length(names) <= @num_to_take ->
        [
          raw("Gives access to #{length(names)} users: ")
          | Air.Utils.CheckboxMapper.highlighted_and_comma_separated(names, @num_to_take)
        ]

      names ->
        [
          raw("Gives access to #{length(names)} users, including: ")
          | Air.Utils.CheckboxMapper.highlighted_and_comma_separated(names, @num_to_take)
        ]
    end
  end

  defp total_columns(tables), do: tables |> Enum.map(&length(&1["columns"])) |> Enum.sum()

  defp total_analyzed(tables), do: tables |> Enum.map(& &1["columns"]) |> List.flatten() |> analyzed()

  defp total_failed(tables), do: tables |> Enum.map(& &1["columns"]) |> List.flatten() |> analysis_failed()

  defp analyzed(columns) do
    Enum.count(columns, fn column ->
      is_boolean(column["isolated"]) and column["shadow_table"] == "ok"
    end)
  end

  defp analysis_failed(columns) do
    Enum.count(columns, fn column ->
      column["isolated"] == "failed" or column["shadow_table"] == "failed"
    end)
  end
end
