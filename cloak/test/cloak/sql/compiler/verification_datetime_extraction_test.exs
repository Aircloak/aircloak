defmodule Cloak.Sql.Compiler.VerificationDatetimeExtraction.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "Condition affected by datetime extractors are forbidden when a constant is involved" do
    test "it is forbidden to cast a date to text and then use it in a WHERE inequality" do
      query = """
      SELECT value FROM (
        SELECT uid, length(cast(column as text)) as value
        FROM table
      ) t
      WHERE value >= 10 and value < 20
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "it is forbidden to cast a date to text and then use it in a WHERE equality" do
      query = """
      SELECT value FROM (
        SELECT uid, cast(column as text) as value
        FROM table
      ) t
      WHERE value = '2017-01-19'
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "it is forbidden to cast a date to text and then use it in a HAVING inequality" do
      query = """
      SELECT value FROM (
        SELECT uid, value, count(*)
        FROM (
          SELECT uid, length(cast(column as text)) as value
          FROM table
        ) t
        GROUP BY uid, value
        HAVING value >= 10 and value < 20
      ) t
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "it is forbidden to cast a date to text and then use it in a HAVING equality" do
      query = """
      SELECT value FROM (
        SELECT uid, value, count(*)
        FROM (
          SELECT uid, cast(column as text) as value
          FROM table
        ) t
        GROUP BY uid, value
        HAVING value = '2017-01-19'
      ) t
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "it is forbidden to cast a date to text and then use it in a JOIN inequality" do
      query = """
      SELECT value FROM (
        SELECT uid, length(cast(column as text)) as value
        FROM table
      ) t INNER JOIN table ON table.uid = t.uid and t.value >= 0 and t.value < 10
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "it is forbidden to cast a date to text and then use it in a JOIN equality" do
      query = """
      SELECT value FROM (
        SELECT uid, cast(column as text) as value
        FROM table
      ) t INNER JOIN table ON table.uid = t.uid and t.value = '2017-01-19'
      """
      refute condition_columns_have_valid_transformations(query)
    end

    Enum.each(~w(year quarter month day hour minute second weekday), fn(extractor_fun) ->
      test "it is forbidden to use the result of function #{extractor_fun} in a WHERE inequality" do
        query = """
        SELECT value FROM (
          SELECT uid, #{unquote(extractor_fun)}(column) as value
          FROM table
        ) t
        WHERE value >= 10 and value < 20
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "it is forbidden to use the result of function #{extractor_fun} in a WHERE equality" do
        query = """
        SELECT value FROM (
          SELECT uid, #{unquote(extractor_fun)}(column) as value
          FROM table
        ) t
        WHERE value = 1
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "it is forbidden to use the result of function #{extractor_fun} in a HAVING inequality" do
        query = """
        SELECT value FROM (
          SELECT uid, value, count(*)
          FROM (
            SELECT uid, #{unquote(extractor_fun)}(column) as value
            FROM table
          ) t
          GROUP BY uid, value
          HAVING value >= 10 and value < 20
        ) t
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "it is forbidden to use the result of function #{extractor_fun} in a HAVING equality" do
        query = """
        SELECT value FROM (
          SELECT uid, value, count(*)
          FROM (
            SELECT uid, #{unquote(extractor_fun)}(column) as value
            FROM table
          ) t
          GROUP BY uid, value
          HAVING value = 1
        ) t
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "it is forbidden to use the result of function #{extractor_fun} in a JOIN inequality" do
        query = """
        SELECT value FROM (
          SELECT uid, #{unquote(extractor_fun)}(column) as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid and t.value >= 0 and t.value < 10
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "it is forbidden to use the result of function #{extractor_fun} in a JOIN equality" do
        query = """
        SELECT value FROM (
          SELECT uid, #{unquote(extractor_fun)}(column) as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid and t.value = 1
        """
        refute condition_columns_have_valid_transformations(query)
      end
    end)
  end

  describe "Condition affected by datetime extractors are allowed when compared against non-constant entities" do
    test "it is OK to cast a date to text and then use it in a WHERE equality when no constant is involved" do
      query = """
      SELECT value FROM (
        SELECT uid, cast(column as text) as value, string
        FROM table
      ) t
      WHERE value = string
      """
      assert condition_columns_have_valid_transformations(query)
    end

    test "it is OK to cast a date to text and then use it in a HAVING equality when no constant is involved" do
      query = """
      SELECT value FROM (
        SELECT uid, value, count(*)
        FROM (
          SELECT uid, cast(column as text) as value, string
          FROM table
        ) t
        GROUP BY uid, value, string
        HAVING value = string
      ) t
      """
      assert condition_columns_have_valid_transformations(query)
    end

    test "it is OK to cast a date to text and then use it in a JOIN equality when no constant is involved" do
      query = """
      SELECT value FROM (
        SELECT uid, cast(column as text) as value
        FROM table
      ) t INNER JOIN table ON table.uid = t.uid and t.value = table.string
      """
      assert condition_columns_have_valid_transformations(query)
    end

    Enum.each(~w(year quarter month day hour minute second weekday), fn(extractor_fun) ->
      test "it is forbidden to use the result of function #{extractor_fun} in a WHERE equality " <>
          "when comparing to a non-constant" do
        query = """
        SELECT value FROM (
          SELECT uid, #{unquote(extractor_fun)}(column) as value, numeric
          FROM table
        ) t
        WHERE value = numeric
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "it is forbidden to use the result of function #{extractor_fun} in a HAVING equality " <>
          "when comparing to a non-constant" do
        query = """
        SELECT value FROM (
          SELECT uid, value, count(*)
          FROM (
            SELECT uid, #{unquote(extractor_fun)}(column) as value, numeric
            FROM table
          ) t
          GROUP BY uid, value, numeric
          HAVING value = numeric
        ) t
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "it is forbidden to use the result of function #{extractor_fun} in a JOIN equality " <>
          "when comparing to a non-constant" do
        query = """
        SELECT value FROM (
          SELECT uid, #{unquote(extractor_fun)}(column) as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid and t.value = table.numeric
        """
        refute condition_columns_have_valid_transformations(query)
      end
    end)
  end

  defp condition_columns_have_valid_transformations(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      {:error, reason} ->
        cond do
          reason =~ ~r/Only unmodified database columns can be limited by a range./ -> false
          reason =~ ~r/functions that extract a component of a date/ -> false
          true -> raise "Compilation failed with other reason than illegal filtering condition: #{inspect reason}"
        end
    end
  end

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Cloak.DataSource.Table.new("table", "uid",
          db_name: "table",
          columns: [
            Table.column("uid", :integer),
            Table.column("column", :datetime),
            Table.column("numeric", :integer),
            Table.column("float", :real),
            Table.column("string", :text)
          ]
        )
      }
    }
  end
end
