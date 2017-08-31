defmodule Cloak.Sql.Compiler.VerificationConditionClauses.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "Rejects conditions used in subqueries" do
    test "math on a column used in a JOIN condition in top-level query" do
      query = """
        SELECT count(*)
        FROM (
          SELECT uid, numeric + 1 as value
          FROM table
        ) t1 INNER JOIN (
          SELECT uid, numeric
          FROM table
        ) t2 ON t1.uid = t2.uid and t1.value >= 10 and t1.value < 20
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "math on a column used in a JOIN condition in subquery" do
      query = """
        SELECT count(*) FROM (
          SELECT t1.uid
          FROM (
            SELECT uid, numeric + 1 as value
            FROM table
          ) t1 INNER JOIN (
            SELECT uid, numeric
            FROM table
          ) t2 ON t1.uid = t2.uid and t1.value >= 10 and t1.value < 20
        ) t
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "math on a column used in a HAVING clause in subquery" do
      query = """
        SELECT count(*) FROM (
          SELECT uid, value, count(*)
          FROM (
            SELECT uid, numeric + 1 as value
            FROM table
          ) t
          GROUP BY uid, value
          HAVING value >= 0 and value < 10
        ) t
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "math on a column used in a HAVING clause in main query is allowed" do
      query = """
        SELECT value, count(*) FROM (
          SELECT uid, numeric + 1 as value
          FROM table
        ) t
        GROUP BY value
        HAVING value > 10
      """
      assert condition_columns_have_valid_transformations(query)
    end
  end

  describe "WHERE-equalities on processed columns" do
    test "unsafe discontinuity is allowed in equality" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            length(btrim(string, 'constant')) as value
          FROM table
        ) t
        WHERE value = 0
      """
      assert condition_columns_have_valid_transformations(query)
    end

    test "on a cast column is allowed in equality" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            cast(string as integer) as value
          FROM table
        ) t
        WHERE value = 10
      """
      assert condition_columns_have_valid_transformations(query)
    end

    test "affected by math is allowed in equality" do
      query = """
        SELECT value FROM (
          SELECT uid, numeric + 2 as value
          FROM table
        ) t
        WHERE value = 10
      """
      assert condition_columns_have_valid_transformations(query)
    end
  end

  describe "Condition-inequalities affected by dangerous math OR discontinuity are forbidden" do
    Enum.each(~w(abs ceil floor round trunc sqrt), fn(discontinuous_function) ->
      test "#{discontinuous_function} without constant used in WHERE" do
        query = """
          SELECT value FROM (
            SELECT uid, #{unquote(discontinuous_function)}(numeric) as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        assert condition_columns_have_valid_transformations(query)
      end
    end)

    Enum.each(~w(div mod), fn(discontinuous_function) ->
      test "#{discontinuous_function} with constant" do
        query = """
          SELECT numeric FROM (
            SELECT
              uid,
              numeric,
              #{unquote(discontinuous_function)}(numeric, 3) as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        refute condition_columns_have_valid_transformations(query)
      end

      test "#{discontinuous_function} without constant" do
        query = """
          SELECT value FROM (
            SELECT uid, #{unquote(discontinuous_function)}(numeric, numeric) as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        assert condition_columns_have_valid_transformations(query)
      end
    end)

    Enum.each(~w(+ - / * ^), fn(math_function) ->
      test "#{math_function} without constant" do
        query = """
          SELECT value FROM (
            SELECT uid, numeric #{unquote(math_function)} numeric as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        assert condition_columns_have_valid_transformations(query)
      end
    end)

    # Note: we can't test with / because it is treated as a dangerous discontinuous
    # function as soon as a constant is involved, and hence halts query compilation.
    Enum.each(~w(+ - * ^), fn(math_function) ->
      test "#{math_function} with constant" do
        query = """
          SELECT value FROM (
            SELECT uid, numeric #{unquote(math_function)} 3 as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        refute condition_columns_have_valid_transformations(query)
      end
    end)

    test "string and constant converted to number" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            length(btrim(string, 'constant')) as value
          FROM table
        ) t
        WHERE value >= 0 and value < 10
      """
      refute condition_columns_have_valid_transformations(query)
    end

    test "dangerous cast column" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            cast(btrim(string, 'constant') as integer) as value
          FROM table
        ) t
        WHERE value >= 0 and value < 10
      """
      refute condition_columns_have_valid_transformations(query)
    end
  end

  defp condition_columns_have_valid_transformations(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      {:error, reason} ->
        if reason =~ ~r/Inequality clauses used to filter the data/ do
          false
        else
          if reason =~ ~r/Equality clauses \(like WHERE/ do
            false
          else
            raise "Compilation failed with other reason than illegal filtering condition: #{inspect reason}"
          end
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
