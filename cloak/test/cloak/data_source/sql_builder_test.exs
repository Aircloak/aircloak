defmodule Cloak.DataSource.SqlBuilderTest do
  use ExUnit.Case, async: true

  use ExUnitProperties
  alias Cloak.DataSource.{SqlBuilder, Table}
  alias Cloak.Sql.Query
  import Cloak.Test.QueryHelpers
  import StreamData

  doctest SqlBuilder

  test "non-text column is not force casted",
    do: refute(sql_string("select int from table") =~ ~r/CAST\("table"\."int".*?text\)/)

  describe "quote_table_name" do
    property "quotes each part" do
      check all(
              quote_char <- one_of([constant(?"), constant(?`)]),
              parts_data <- parts_data(),
              {parts, unquoted_parts} = Enum.unzip(parts_data),
              table_name = Enum.join(parts, ".")
            ) do
        expected = unquoted_parts |> Stream.map(&to_string([quote_char, &1, quote_char])) |> Enum.join(".")
        assert SqlBuilder.quote_table_name(table_name, quote_char) == expected
      end
    end
  end

  describe "table_name_parts" do
    property "returns the list of unquoted parts" do
      check all(
              parts_data <- parts_data(),
              {parts, unquoted_parts} = Enum.unzip(parts_data),
              table_name = Enum.join(parts, ".")
            ) do
        assert SqlBuilder.table_name_parts(table_name) == unquoted_parts
      end
    end

    property "raises on invalid inputs" do
      check all(
              valid_parts <- valid_parts(),
              invalid_pos <- integer(0..length(valid_parts)),
              invalid_part <- invalid_part(),
              table_name = valid_parts |> List.insert_at(invalid_pos, invalid_part) |> Enum.join(".")
            ) do
        assert_raise ArgumentError, fn -> SqlBuilder.table_name_parts(table_name) end
      end
    end
  end

  test "build userid join string" do
    tables = %{
      uid_table: %{
        name: "uid_table",
        user_id: "uid",
        db_name: "uid_table",
        columns: [
          Table.column("uid", :integer),
          Table.column("string", :text),
          Table.column("key2", :integer)
        ],
        keys: %{"key" => :key},
        user_id_join_chain: []
      },
      no_uid_table: %{
        name: "no_uid_table",
        user_id: nil,
        db_name: "no_uid_table",
        columns: [
          Table.column("key1", :integer)
        ],
        keys: %{"key" => :key},
        user_id_join_chain: [{"key1", :uid_table, "key2"}]
      }
    }

    assert SqlBuilder.build_table_chain_with_user_id(tables, :uid_table) == {~s("uid_table"."uid"), ~s("uid_table")}

    assert SqlBuilder.build_table_chain_with_user_id(tables, :no_uid_table) ==
             {~s("uid_table"."uid"),
              ~s("no_uid_table" INNER JOIN "uid_table" ON "no_uid_table"."key1" = "uid_table"."key2")}
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp sql_string(query, dialect \\ PostgreSQL) do
    compiled_query =
      query
      |> compile!(data_source(Module.concat(Cloak.DataSource, dialect)))
      |> Query.resolve_db_columns()

    SqlBuilder.build(compiled_query)
  end

  defp data_source(driver) do
    %{
      name: "data_source",
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

  defp unquoted_part_data(), do: map(nonempty_string_of(regular_char()), &{&1, &1})

  defp quoted_part_data() do
    map(
      nonempty_string_of(one_of([regular_char(), special_char()])),
      &{~s/"#{String.replace(&1, ~s/"/, ~s/""/)}"/, &1}
    )
  end

  @special_chars [?., ?"]

  defp regular_char() do
    frequency([
      {20, [?a..?z, ?A..?Z, ?0..?9] |> Enum.map(&integer/1) |> one_of()},
      {10, constant(?\s)},
      {1, filter(integer(0..0xD7FF), &(&1 not in @special_chars))}
    ])
  end

  defp special_char(), do: @special_chars |> Enum.map(&constant/1) |> one_of()

  defp nonempty_string_of(char_generator), do: map(nonempty(list_of(char_generator)), &to_string/1)

  defp invalid_part(), do: one_of([empty_unquoted(), non_closed_quote(), unquoted_special_char()])

  defp empty_unquoted(), do: constant("")

  defp non_closed_quote(),
    do: map(quoted_part_data(), fn {quoted, _unquoted} -> String.replace(quoted, ~r/"$/, "") end)

  defp unquoted_special_char() do
    bind(list_of(regular_char()), fn regular_chars ->
      bind(special_char(), fn special_char ->
        map(special_char_insert_pos(special_char, regular_chars), fn insert_pos ->
          regular_chars |> List.insert_at(insert_pos, special_char) |> to_string()
        end)
      end)
    end)
  end

  defp special_char_insert_pos(?., regular_chars), do: one_of([constant(0), constant(length(regular_chars))])
  defp special_char_insert_pos(_char, regular_chars), do: integer(0..length(regular_chars))
end
