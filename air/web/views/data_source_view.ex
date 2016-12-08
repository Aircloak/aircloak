defmodule Air.DataSourceView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.{Schemas.DataSource, DataSourceManager}

  defp available?(data_source) do
    DataSourceManager.available?(data_source.global_id)
  end

  defp number_of_tables(data_source) do
    length(DataSource.tables(data_source))
  end

  defp sample_of_tables(data_source) do
    DataSource.tables(data_source)
    |> Enum.take(3)
    |> Enum.map(fn(%{"id" => name}) -> name end)
    |> Enum.join(", ")
  end

  defp to_json(map) do
    {:safe, Poison.encode!(map)}
  end

  defp selectables(conn, data_source, views) do
    DataSource.tables(data_source) ++
      Enum.map(views, &%{
        id: &1.name,
        columns: Map.fetch!(&1.result_info, "columns"),
        edit_link: data_source_view_path(conn, :edit, &1.data_source_id, &1.id),
        delete_html:
          safe_to_string(link("delete",
            to: data_source_view_path(conn, :delete, &1.data_source_id, &1.id),
            method: :delete,
            "data-confirm": "Delete #{&1.name}?",
            class: "btn btn-danger btn-xs"
          ))
      })
  end
end
