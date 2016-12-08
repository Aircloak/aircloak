defmodule Air.DataSourceView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.{Schemas.DataSource, DataSourceManager}

  def available?(data_source) do
    DataSourceManager.available?(data_source.global_id)
  end

  def number_of_tables(data_source) do
    length(DataSource.tables(data_source))
  end

  def sample_of_tables(data_source) do
    DataSource.tables(data_source)
    |> Enum.take(3)
    |> Enum.map(fn(%{"id" => name}) -> name end)
    |> Enum.join(", ")
  end

  def to_json(map) do
    {:safe, Poison.encode!(map)}
  end

  def tables(data_source) do
    {:safe, data_source |> DataSource.tables() |> Poison.encode!()}
  end

  defp views_for_client(conn, views) do
    Enum.map(views, &%{name: &1.name, result_info: &1.result_info,
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
