defmodule Air.PsqlServer.ShadowDb.Schema do
  @moduledoc "Creation of shadow database schema."

  import Ecto.Query

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a list of CREATE TABLE statements for all selectables in the data source."
  @spec build(User.t(), String.t()) :: [String.t()]
  def build(user, data_source_name) do
    data_source_tables(user, data_source_name)
    |> Enum.reject(& &1.broken?)
    |> Enum.map(&~s/CREATE TABLE "#{sanitize_name(&1.id)}" (#{columns_sql(&1.columns)});/)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp data_source_tables(user, data_source_name) do
    case Air.Service.DataSource.by_name(data_source_name) do
      nil ->
        []

      data_source ->
        regular_tables =
          data_source
          |> Air.Schemas.DataSource.tables()
          |> normalize_tables()

        views =
          from(
            view in Air.Schemas.View,
            where: view.user_id == ^user.id and view.data_source_id == ^data_source.id,
            select: view
          )
          |> Air.Repo.all()
          |> Enum.map(&normalize_selectable/1)

        analyst_tables =
          from(
            analyst_table in Air.Schemas.AnalystTable,
            where: analyst_table.user_id == ^user.id and analyst_table.data_source_id == ^data_source.id,
            select: analyst_table
          )
          |> Air.Repo.all()
          |> Enum.map(&normalize_selectable/1)

        Enum.concat([regular_tables, views, analyst_tables])
    end
  end

  defp normalize_tables(tables) do
    tables
    |> Stream.map(&normalize_table/1)
    |> Enum.reject(&Enum.empty?(&1.columns))
  end

  defp normalize_table(table) do
    %{
      id: Map.fetch!(table, "id"),
      columns: table |> Map.get("columns", []) |> Enum.map(&normalize_column/1),
      broken?: false
    }
  end

  defp normalize_selectable(selectable) do
    %{
      id: selectable.name,
      columns: selectable.columns,
      broken?: selectable.broken or Enum.empty?(selectable.columns)
    }
  end

  defp normalize_column(%{"name" => name, "type" => type}), do: %{name: name, type: type}

  defp columns_sql(columns), do: columns |> Enum.map(&column_sql/1) |> Enum.join(", ")

  defp column_sql(column), do: ~s/"#{sanitize_name(column.name)}" #{type_sql(column.type)}/

  defp type_sql("boolean"), do: "boolean"
  defp type_sql("real"), do: "real"
  defp type_sql("integer"), do: "integer"
  defp type_sql("text"), do: "text"
  defp type_sql("date"), do: "date"
  defp type_sql("time"), do: "time without time zone"
  defp type_sql("datetime"), do: "timestamp without time zone"
  defp type_sql("unknown"), do: "text"

  defp sanitize_name(name), do: Regex.replace(~r/"/, name, ~s/""/)
end
