defmodule Cloak.DataSource.SqlBuilderTest do
  use ExUnit.Case, async: true

  use ExUnitProperties
  alias Cloak.DataSource.{SqlBuilder, Table}
  alias Cloak.Sql.Query
  import Cloak.Test.QueryHelpers
  import StreamData

  test "non-text column is not force casted",
    do: refute(sql_string("select int from table") =~ ~r/CAST\("table"\."int"/)

  test "workaround for text comparisons on SQL Server ignoring trailing spaces",
    do: assert(sql_string("select count(*) from table where string = 'ab'", SQLServer) =~ "= N'ab.')")

  describe "quote_table_name" do
    property "quotes each part" do
      check all quote_char <- constant_of([?", ?']),
                parts_data <- parts_data(),
                {parts, unquoted_parts} = Enum.unzip(parts_data),
                table_name = Enum.join(parts, ".") do
        expected = unquoted_parts |> Stream.map(&to_string([quote_char, &1, quote_char])) |> Enum.join(".")
        assert SqlBuilder.quote_table_name(table_name, quote_char) == expected
      end
    end
  end

  describe "table_name_parts" do
    property "returns the list of unquoted parts" do
      check all parts_data <- parts_data(),
                {parts, unquoted_parts} = Enum.unzip(parts_data),
                table_name = Enum.join(parts, ".") do
        assert SqlBuilder.table_name_parts(table_name) == unquoted_parts
      end
    end

    property "raises on invalid inputs" do
      check all valid_parts <- valid_parts(),
                invalid_pos <- integer(0..length(valid_parts)),
                invalid_part <- invalid_part(),
                table_name = valid_parts |> List.insert_at(invalid_pos, invalid_part) |> Enum.join(".") do
        assert_raise ArgumentError, fn -> SqlBuilder.table_name_parts(table_name) end
      end
    end
  end

  defp sql_string(query, dialect \\ PostgreSQL) do
    compiled_query =
      query
      |> compile!(data_source(Module.concat(Cloak.DataSource, dialect)))
      |> Query.resolve_db_columns()

    SqlBuilder.build(compiled_query)
  end

  defp data_source(driver) do
    %{
      driver: driver,
      tables: %{
        table:
          Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("string", :text),
              Table.column("int", :integer)
            ]
          )
      }
    }
  end

  # -------------------------------------------------------------------
  # Generators for table names
  # -------------------------------------------------------------------

  defp valid_parts(), do: map(parts_data(), &Enum.map(&1, fn {valid_part, _unqouted_part} -> valid_part end))

  defp parts_data(), do: nonempty(list_of(one_of([unquoted_part_data(), quoted_part_data()])))

  defp unquoted_part_data(), do: map(nonempty_string_of(unquoted_char()), &{&1, &1})

  defp quoted_part_data() do
    map(
      nonempty_string_of(one_of([unquoted_char(), special_char()])),
      &{~s/"#{&1}"/, String.replace(&1, ~s/""/, ~s/"/)}
    )
  end

  defp unquoted_char(), do: constant_of(Enum.concat([?a..?z, ?A..?Z, ?0..?9, [?_]]))

  defp special_char() do
    frequency([
      {2, constant_of([?\s, ~s/""/, ?', ?.])},
      {1, constant_of(0xA0..0xD7FF)}
    ])
  end

  defp constant_of(elements), do: elements |> Enum.map(&constant/1) |> one_of()

  defp nonempty_string_of(char_generator), do: map(nonempty(list_of(char_generator)), &to_string/1)

  defp invalid_part(), do: one_of([empty_unquoted(), non_closed_quote()])

  defp empty_unquoted(), do: constant("")

  defp non_closed_quote(),
    do: map(quoted_part_data(), fn {quoted, _unquoted} -> String.replace(quoted, ~r/"$/, "") end)
end
