defmodule Cloak.Sql.Compiler.VerificationCrashingFunctions.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  describe "/ and sqrt can crash irrespective of where they are used" do
    test "/ is allowed when selected in subquery purely with constant" do
      query = """
      SELECT value FROM (
        SELECT uid, numeric / 1, numeric as value
        FROM table
      ) t
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is allowed when selected only in a subquery purely with constant" do
      query = """
      SELECT count(*) FROM (
        SELECT uid, value FROM (
          SELECT uid, numeric / 1, numeric as value
          FROM table
        ) t
      ) t
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is allowed when used in JOIN match condition purely with constant" do
      query = """
      SELECT value FROM (
        SELECT uid, numeric / 1 as calc_val, numeric as value
        FROM table
      ) t INNER JOIN table ON table.uid = t.uid and t.calc_val = 0
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is allowed when used in JOIN order condition purely with constant" do
      query = """
      SELECT value FROM (
        SELECT uid, numeric / 1 as calc_val, numeric as value
        FROM table
      ) t INNER JOIN table ON table.uid = t.uid and t.calc_val >= 0 and t.calc_val < 10
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is allowed when used in HAVING match condition purely with constant" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid, value FROM (
          SELECT uid, numeric / 1 as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        GROUP BY t.uid, value, calc_val
        HAVING calc_val = 0
      ) t
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is allowed when used in HAVING order condition purely with constant" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid, value FROM (
          SELECT uid, numeric / 1 as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        GROUP BY t.uid, value, calc_val
        HAVING calc_val >= 0 and calc_val < 10
      ) t
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is allowed when used in WHERE match condition purely with constant" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid FROM (
          SELECT uid, numeric / 1 as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        WHERE calc_val = 10
      ) t
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is allowed when used in WHERE order condition with constant" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid FROM (
          SELECT uid, numeric / 1 as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        WHERE calc_val >= 0 and calc_val < 10
      ) t
      """

      assert compiles_without_potential_crash_error(query)
    end

    test "/ is not allowed when selected in subquery with constant infected expression" do
      query = """
      SELECT value FROM (
        SELECT uid, numeric / (numeric + 1), numeric as value
        FROM table
      ) t
      """

      assert expressions_potentially_crash(query)
    end

    test "/ is not allowed when selected only in a subquery with constant infected expression, " <>
           "but not top-level-query" do
      query = """
      SELECT count(*) FROM (
        SELECT uid, value FROM (
          SELECT uid, numeric / (numeric + 1), numeric as value
          FROM table
        ) t
      ) t
      """

      assert expressions_potentially_crash(query)
    end

    test "/ is not allowed when used in JOIN match condition with constant infected expression" do
      query = """
      SELECT value FROM (
        SELECT uid, numeric / (numeric + 1) as calc_val, numeric as value
        FROM table
      ) t INNER JOIN table ON table.uid = t.uid and t.calc_val = 0
      """

      assert expressions_potentially_crash(query)
    end

    test "/ is not allowed when used in JOIN order condition with constant infected expression" do
      query = """
      SELECT value FROM (
        SELECT uid, numeric / (numeric + 1) as calc_val, numeric as value
        FROM table
      ) t INNER JOIN table ON table.uid = t.uid and t.calc_val >= 0 and t.calc_val < 10
      """

      assert expressions_potentially_crash(query)
    end

    test "/ is not allowed when used in HAVING match condition with constant infected expression" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid, value FROM (
          SELECT uid, numeric / (numeric + 1) as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        GROUP BY t.uid, value, calc_val
        HAVING calc_val = 0
      ) t
      """

      assert expressions_potentially_crash(query)
    end

    test "/ is not allowed when used in HAVING order condition with constant infected expression" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid, value FROM (
          SELECT uid, numeric / (numeric + 1) as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        GROUP BY t.uid, value, calc_val
        HAVING calc_val >= 0 and calc_val < 10
      ) t
      """

      assert expressions_potentially_crash(query)
    end

    test "/ is not allowed when used in WHERE match condition with constant infected expression" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid FROM (
          SELECT uid, numeric / (numeric + 1) as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        WHERE calc_val = 10
      ) t
      """

      assert expressions_potentially_crash(query)
    end

    test "/ not is allowed when used in WHERE order condition with constant infected expression" do
      query = """
      SELECT count(*) FROM (
        SELECT t.uid FROM (
          SELECT uid, numeric / (numeric + 1) as calc_val, numeric as value
          FROM table
        ) t INNER JOIN table ON table.uid = t.uid
        WHERE calc_val >= 0 and calc_val < 10
      ) t
      """

      assert expressions_potentially_crash(query)
    end
  end

  test "/ is treated as a potentially crashing function if divisor is touched by a constant" do
    query = "SELECT numeric / (numeric * 2) FROM table"
    assert expressions_potentially_crash(query)
  end

  test "/ is not a potentially crashing function if divisor is a pure constant" do
    query = "SELECT numeric / 2 FROM table"
    assert compiles_without_potential_crash_error(query)
  end

  defp expressions_potentially_crash(query) do
    case compile(query, data_source()) do
      {:ok, _} ->
        raise "Expected query compilation failure due to potentially crashing function usage."

      {:error, reason} ->
        if reason =~ ~r/database exception/ do
          true
        else
          raise "Compilation failed with other reason than illegal potential crash: #{inspect(reason)}"
        end
    end
  end

  defp compiles_without_potential_crash_error(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      {:error, reason} -> not (reason =~ ~r/database exception/)
    end
  end

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
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
