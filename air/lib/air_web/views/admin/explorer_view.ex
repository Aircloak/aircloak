defmodule AirWeb.Admin.ExplorerView do
  use Air.Web, :view

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

  defp status_to_badge_class(status) do
    case status do
      :new -> "badge-primary"
      :processing -> "badge-info"
      :complete -> "badge-success"
      :error -> "badge-danger"
      :canceled -> "badge-secondary"
    end
  end

  defp metrics(analysis) do
    Enum.map(Jason.decode!(analysis.metrics), fn %{"name" => k, "value" => v} -> {k, v} end)
  end

  defp to_pretty_json(val) do
    Jason.encode!(val, pretty: true)
  end

  defp by_table(analyses) do
    Enum.group_by(analyses, & &1.table_name)
  end
end
