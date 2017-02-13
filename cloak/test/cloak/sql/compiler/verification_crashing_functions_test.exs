defmodule Cloak.Sql.Compiler.VerificationCrashingFunctions.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Compiler, Parser}
  alias Cloak.Query.Error

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
      {:ok, _} -> raise "Expected query compilation failure due to potentially crashing function usage."
      %Error{human_description: reason} ->
        if reason =~ ~r/database exception/ do
          true
        else
          raise "Compilation failed with other reason than illegal potential crash: #{inspect reason}"
        end
    end
  end

  defp compiles_without_potential_crash_error(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      %Error{human_description: reason} -> not (reason =~ ~r/database exception/)
    end
  end

  defp compile(query_string, data_source, options \\ []) do
    query = Parser.parse!(query_string)
    Compiler.compile(data_source, query, Keyword.get(options, :parameters, []),
      Keyword.get(options, :views, %{}))
  end

  defp data_source(driver \\ Cloak.DataSource.PostgreSQL) do
    %{driver: driver, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [
          {"uid", :integer}, {"column", :datetime}, {"numeric", :integer}, {"float", :real}, {"string", :text}
        ],
        projection: nil
      },
    }}
  end
end
