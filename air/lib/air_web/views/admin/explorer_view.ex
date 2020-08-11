defmodule AirWeb.Admin.ExplorerView do
  use Air.Web, :view
  alias Air.Service.Explorer

  defp checkbox_mapper(form, field, input_opts, {data_source, selected_tables}, label_opts, _opts) do
    tables =
      Explorer.elligible_tables_for_datasource(data_source)
      |> Enum.map(&{&1, &1})

    checked = Keyword.get(input_opts, :checked, false)

    content_tag(:div, class: "form-check") do
      [
        tag(:input, [
          {:class, "form-check-input"},
          {:"data-target", "#tables-#{data_source.id}"},
          {:"data-toggle", "collapse"} | input_opts
        ]),
        label(form, field, [{:class, "form-check-label"} | label_opts]) do
          data_source.name
        end,
        content_tag(:div, class: "collapse #{if checked, do: "show"} nested-checkboxes", id: "tables-#{data_source.id}") do
          content_tag(:div) do
            PhoenixMTM.Helpers.collection_checkboxes(form, :tables, tables,
              selected: selected_tables,
              mapper: &table_checkbox_mapper/6,
              data_source_id: data_source.id
            )
          end
        end
      ]
    end
  end

  defp table_checkbox_mapper(form, field, input_opts, table_name, label_opts, opts) do
    data_source_id = Keyword.get(opts, :data_source_id)

    input_opts =
      input_opts
      |> Keyword.update!(:name, fn name -> name <> "[#{data_source_id}]" end)
      |> Keyword.put(:class, "form-check-input")

    content_tag(:div, class: "form-check") do
      [
        tag(:input, input_opts),
        label(form, field, [{:class, "form-check-label text-break mw-100"} | label_opts]) do
          table_name
        end
      ]
    end
  end

  defp version_link(commit) do
    link(String.slice(commit, 0, 8), to: "https://github.com/diffix/explorer/tree/#{commit}")
  end

  defp status_to_badge_class(status) do
    case status do
      :new -> "badge-primary"
      :processing -> "badge-info"
      :complete -> "badge-success"
      :error -> "badge-danger"
      :canceled -> "badge-secondary"
    end
  end

  defp to_pretty_json(val) do
    Jason.encode!(val, pretty: true)
  end

  defp time_ago(date_time) do
    content_tag(:time, Timex.from_now(date_time),
      datetime: NaiveDateTime.to_iso8601(date_time),
      title: format_date(date_time)
    )
  end

  defp admin_explorer_failed_queries_path(%{assigns: %{analyses: analyses, data_source: data_source}} = conn) do
    non_empty_analyses = analyses |> Enum.reject(&is_nil/1)

    admin_query_path(conn, :failed,
      users: [Explorer.user().id],
      data_sources: [data_source.id],
      from: non_empty_analyses |> Enum.map(& &1.inserted_at) |> Enum.min(fn -> nil end) |> format_date(),
      to: non_empty_analyses |> Enum.map(& &1.updated_at) |> Enum.max(fn -> nil end) |> format_date()
    )
  end

  defp format_date(nil), do: nil
  defp format_date(d), do: Timex.format!(d, "{ISOdate} {ISOtime}")
end
