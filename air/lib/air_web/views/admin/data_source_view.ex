defmodule AirWeb.Admin.DataSourceView do
  @moduledoc false
  use Air.Web, :view

  alias Air.Repo
  alias Air.Service.{DataSource.Column, AnalystTable}

  defdelegate availability_label(data_source), to: AirWeb.DataSourceView
  defdelegate number_of_tables(data_source), to: AirWeb.DataSourceView

  def nil_or_empty?(nil), do: true
  def nil_or_empty?(""), do: true
  def nil_or_empty?(_), do: false

  def number_of_analyst_tables(data_source), do: length(AnalystTable.all_for_data_source(data_source))

  def available?(data_source), do: Air.Service.DataSource.available?(data_source.name)

  defp checkbox_mapper(form, field, input_opts, group, label_opts, _opts) do
    content_tag(:div, class: "form-check") do
      [
        tag(:input, [{:class, "form-check-input"} | input_opts]),
        label(form, field, [{:class, "form-check-label"} | label_opts]) do
          [Air.Utils.CheckboxMapper.group_label_text(group), raw(" &ndash; ") | users_given_access_to(group)]
        end
      ]
    end
  end

  defp link_unless_explorer(conn, title, thing, opts) do
    if thing.system && thing.name == "Diffix Explorer" do
      link("Manage", Keyword.replace(opts, :to, admin_explorer_path(conn, :index)))
    else
      link(title, opts)
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
          raw("Gives access to #{length(names)} #{Inflex.inflect("user", length(names))}: ")
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

  defp total_pending(tables), do: tables |> Enum.map(& &1["columns"]) |> List.flatten() |> analysis_pending()

  defp total_failed(tables), do: tables |> Enum.map(& &1["columns"]) |> List.flatten() |> analysis_failed()

  defp analyzed(columns), do: Enum.count(columns, &Column.analyzed_successfully?/1)

  defp analysis_pending(columns), do: Enum.count(columns, &Column.analysis_pending?/1)

  defp analysis_failed(columns), do: Enum.count(columns, &Column.analysis_failed?/1)

  defp cloak_infos_for_data_source(data_source),
    do: Air.Service.Cloak.cloak_infos_for_data_source(data_source.name)

  defp number_of_cloaks_serving(data_source), do: length(cloak_infos_for_data_source(data_source))
end
