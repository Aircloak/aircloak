defmodule Compliance.CommentsTest do
  use ComplianceCase, async: true

  test "table comments are preserved", context do
    context
    |> data_sources()
    |> Enum.map(fn data_source ->
      data_source.tables
      |> Map.values()
      |> Enum.filter(&valid_table?/1)
      |> Enum.each(fn table ->
        table_comment = get_in(table, [:comments, :table])

        if table.name == "notes" do
          assert table_comment == "Overridden comment on table notes.",
                 "Data source #{data_source.driver} does not properly override comment for table notes"
        else
          assert table_comment == "This is table #{db_name(table)}.",
                 "Data source #{data_source.driver} is missing comment for table #{db_name(table)}"
        end
      end)
    end)
  end

  test "column comments are preserved", context do
    context
    |> data_sources()
    |> Enum.each(fn data_source ->
      data_source.tables
      |> Map.values()
      |> Enum.filter(&valid_table?/1)
      |> Enum.each(fn table ->
        column_comments = get_in(table, [:comments, :columns]) || %{}

        Enum.each(table.columns, fn column ->
          column_comment = column_comments[column.name]

          if table.name == "notes" and column.name == "title" do
            assert column_comment == "Overridden comment on column notes.title.",
                   "Data source #{data_source.driver} does not properly override comment for column notes.title"
          else
            assert column_comment == "This is column #{column.name}.",
                   "Data source #{data_source.driver} is missing comment for column #{db_name(table)}.#{column.name}"
          end
        end)
      end)
    end)
  end

  defp valid_table?(table), do: table.db_name != nil and table.query == nil

  defp db_name(table) do
    case Cloak.DataSource.SqlBuilder.table_name_parts(table.db_name) do
      [_, table_name] -> table_name
      [table_name] -> table_name
    end
  end

  defp data_sources(context) do
    context
    |> disable_for(Cloak.DataSource.MongoDB)
    |> Map.fetch!(:data_sources)
  end
end
