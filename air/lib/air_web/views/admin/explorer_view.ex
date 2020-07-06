defmodule AirWeb.Admin.ExplorerView do
  use Air.Web, :view
  alias Air.Service.Explorer

  defp checkbox_mapper(form, field, input_opts, {name, _description}, label_opts, _opts) do
    content_tag(:div, class: "form-check") do
      [
        tag(:input, [{:class, "form-check-input"} | input_opts]),
        label(form, field, [{:class, "form-check-label"} | label_opts]) do
          name
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

  defp admin_explorer_failed_queries_path(%{assigns: %{analyses: analyses, data_source: data_source}} = conn),
    do:
      admin_query_path(conn, :failed,
        users: [Explorer.user().id],
        data_sources: [data_source.id],
        from: analyses |> Enum.map(& &1.inserted_at) |> Enum.min(fn -> nil end) |> format_date(),
        to: analyses |> Enum.map(& &1.updated_at) |> Enum.max(fn -> nil end) |> format_date()
      )

  defp format_date(nil), do: nil
  defp format_date(d), do: Timex.format!(d, "{ISOdate} {ISOtime}")
end
