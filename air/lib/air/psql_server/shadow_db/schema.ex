defmodule Air.PsqlServer.ShadowDb.Schema do
  @moduledoc "Creation of shadow database schema."

  import Ecto.Query

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a list of CREATE TABLE statements for all selectables in the data source."
  @spec create_table_statements(User.t(), String.t()) :: [String.t()]
  def create_table_statements(user, data_source_name) do
    data_source_tables(user, data_source_name)
    |> Enum.reject(& &1.broken?)
    |> Enum.flat_map(&[table_sql(&1) | comments(&1)])
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp table_sql(table),
    do: ~s/CREATE TABLE "#{sanitize_name(table.id)}" (#{columns_sql(table.columns)});/

  defp comments(table) do
    [table_comment(table) | Enum.map(table.columns, &column_comment(table, &1))]
    |> Enum.reject(&is_nil/1)
  end

  defp table_comment(table) do
    case table.comment do
      nil -> nil
      "" -> nil
      comment -> ~s/COMMENT ON TABLE "#{sanitize_name(table.id)}" IS '#{sanitize_string(comment)}';/
    end
  end

  defp column_comment(table, column) do
    case column.comment do
      nil ->
        nil

      "" ->
        nil

      comment ->
        ~s/COMMENT ON COLUMN "#{sanitize_name(table.id)}"."#{sanitize_name(column.name)}" IS '#{
          sanitize_string(comment)
        }';/
    end
  end

  defp data_source_tables(user, data_source_name) do
    case Air.Service.DataSource.fetch_as_user({:name, data_source_name}, user) do
      {:error, _} ->
        []

      {:ok, data_source} ->
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
    |> Aircloak.atomize_keys()
    |> Enum.map(&normalize_table/1)
    |> Enum.reject(&Enum.empty?(&1.columns))
  end

  defp normalize_table(table) do
    %{
      id: table.id,
      columns: table.columns,
      broken?: false,
      comment: table.comment
    }
  end

  defp normalize_selectable(selectable) do
    %{
      id: selectable.name,
      columns: selectable.columns,
      broken?: selectable.broken or Enum.empty?(selectable.columns),
      comment: selectable.comment
    }
  end

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

  defp sanitize_string(name), do: Regex.replace(~r/'/, name, ~s/''/)
end
