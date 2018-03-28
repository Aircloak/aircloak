defmodule Cloak.Sql.Compiler.TypeChecker.Type.Test do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser, Compiler.TypeChecker.Type}

  describe "records used functions" do
    test "records usage of single functions",
      do: assert(type_first_column("SELECT abs(numeric) FROM table").applied_functions == ["abs"])

    test "records usage of math functions",
      do: assert(type_first_column("SELECT numeric + numeric FROM table").applied_functions == ["+"])

    test "records functions used across subqueries",
      do:
        assert(type_first_column("SELECT c FROM (SELECT abs(numeric) as c FROM table) t").applied_functions == ["abs"])

    test "records multiple functions top down (distinct functions)",
      do:
        assert(
          type_first_column("SELECT abs(numeric + numeric) FROM table").applied_functions == [
            "abs",
            "+"
          ]
        )

    test "records multiple functions top down (function repeats)",
      do:
        assert(
          type_first_column("SELECT abs(numeric + abs(numeric)) FROM table").applied_functions == ["abs", "+", "abs"]
        )
  end

  describe "constant detection" do
    test "not constant if no constant appears", do: refute(constant_involved?("SELECT numeric FROM table"))

    test "not constant if math on two columns", do: refute(constant_involved?("SELECT numeric + numeric FROM table"))

    test "two math operations are considered a constant",
      do: assert(constant_involved?("SELECT numeric + (numeric * numeric) FROM table"))

    test "constant if input to a function is a constant" do
      assert constant?("SELECT abs(1) FROM table")
      assert constant?("SELECT 1 + 1 FROM table")
    end

    test "constant involved if an user provided constant is an input to a function",
      do: assert(constant_involved?("SELECT numeric + 1 FROM table"))

    test "considers two math operations to be the equivalent of there being a constant in the expression",
      do: assert(constant_involved?("SELECT cast(numeric + (numeric * numeric) as integer) FROM table"))
  end

  describe "knows which columns were involved" do
    test "when a column is selected" do
      type = type_first_column("SELECT numeric FROM table")
      assert expression_name(type.history_of_columns_involved) == ["numeric"]
    end

    test "when a column is selected inside a function" do
      type = type_first_column("SELECT abs(numeric) FROM table")
      assert expression_name(type.history_of_columns_involved) == ["numeric"]
    end

    test "when a column is selected in a subquery" do
      type = type_first_column("SELECT col FROM (SELECT uid, numeric as col FROM table) t")
      assert expression_name(type.history_of_columns_involved) == ["numeric"]
    end

    test "when multiple columns are selected" do
      type = type_first_column("SELECT concat(string, cast(numeric as text)) FROM table")
      assert expression_name(type.history_of_columns_involved) == ["string", "numeric"]
    end

    test "deduplicates columns" do
      type = type_first_column("SELECT concat(string, string) FROM table")
      assert expression_name(type.history_of_columns_involved) == ["string"]
    end

    def expression_name(columns), do: Enum.map(columns, & &1.name)
  end

  describe "records a history of restricted functions" do
    test "empty history for queries without functions or math" do
      type = type_first_column("SELECT numeric FROM table")
      assert type.history_of_restricted_transformations == []
    end

    test "for function with discontinuious function pow" do
      type = type_first_column("SELECT pow(numeric, 10) FROM table")
      assert type.history_of_restricted_transformations == [{:restricted_function, "^"}]
    end

    test "even when multiple occur" do
      type = type_first_column("SELECT abs(pow(numeric, 10)) FROM table")

      assert type.history_of_restricted_transformations == [{:restricted_function, "abs"}, {:restricted_function, "^"}]
    end

    test "does not record discontinuous functions when they appear in an un-restricted form" do
      type = type_first_column("SELECT pow(cast(sqrt(numeric) as float), 10) FROM table")
      assert type.history_of_restricted_transformations == [{:restricted_function, "^"}]
    end

    test "records math influenced by a constant as a potential offense" do
      type = type_first_column("SELECT numeric + 10 FROM table")
      assert type.history_of_restricted_transformations == [{:restricted_function, "+"}]
    end

    test "does not record math between non-constant influenced columns" do
      type = type_first_column("SELECT numeric + numeric FROM table")
      assert type.history_of_restricted_transformations == []
    end

    test "records multiple math offenses" do
      type = type_first_column("SELECT numeric + 10 FROM (SELECT uid, numeric - 1 as numeric FROM table) t")

      assert type.history_of_restricted_transformations == [{:restricted_function, "+"}, {:restricted_function, "-"}]
    end

    test "records multiple instances of the same offense" do
      type = type_first_column("SELECT numeric + 10 FROM (SELECT uid, numeric + 1 as numeric FROM table) t")

      assert type.history_of_restricted_transformations == [{:restricted_function, "+"}, {:restricted_function, "+"}]
    end

    test "records restricted functions when it believes a constant has been constructed" do
      type =
        type_first_column("""
          SELECT abs(pow(numeric, numeric) + pow(numeric, numeric)) FROM table
        """)

      assert type.history_of_restricted_transformations == [{:restricted_function, "abs"}, {:restricted_function, "+"}]
    end
  end

  describe "unclear_implicit_range?" do
    test "false when implicit range by itself",
      do: refute(%Type{applied_functions: ["month"]} |> Type.unclear_implicit_range?())

    test "false when no implicit range", do: refute(%Type{applied_functions: []} |> Type.unclear_implicit_range?())

    test "false when in combination with aggregator",
      do:
        refute(
          %Type{applied_functions: ["trunc", "avg", "sum"]}
          |> Type.unclear_implicit_range?()
        )

    test "false when in combination with cast",
      do:
        refute(
          %Type{applied_functions: ["trunc", {:cast, :integer}]}
          |> Type.unclear_implicit_range?()
        )

    test "true when the implicit range operates on an unclear expression",
      do: assert(%Type{applied_functions: ["trunc", "+"]} |> Type.unclear_implicit_range?())

    test "true when the implicit range is later computed on",
      do: assert(%Type{applied_functions: ["+", "trunc"]} |> Type.unclear_implicit_range?())

    test "true when nested implicit ranges",
      do:
        assert(
          %Type{applied_functions: ["date_trunc", "trunc"]}
          |> Type.unclear_implicit_range?()
        )
  end

  describe "clear_column?" do
    test "true for raw columns", do: assert(type_first_column("SELECT numeric FROM table") |> Type.clear_column?())

    test "true for raw columns from subqueries",
      do:
        assert(
          type_first_column("SELECT n FROM (SELECT numeric AS n FROM table) x")
          |> Type.clear_column?()
        )

    test "true for columns with one cast",
      do:
        assert(
          type_first_column("SELECT CAST(numeric AS text) FROM table")
          |> Type.clear_column?()
        )

    test "false for columns with multiple casts",
      do:
        refute(
          type_first_column("SELECT CAST(CAST(numeric AS text) AS real) FROM table")
          |> Type.clear_column?()
        )

    test "false for expressions with more than one column",
      do: refute(type_first_column("SELECT numeric + numeric FROM table") |> Type.clear_column?())

    test "false for processed columns",
      do: refute(type_first_column("SELECT sqrt(numeric) FROM table") |> Type.clear_column?())

    test "ignores allowed functions",
      do:
        assert(
          type_first_column("SELECT sqrt(numeric) FROM table")
          |> Type.clear_column?(["sqrt"])
        )

    test "does not ignore nested, not allowed functions",
      do:
        refute(
          type_first_column("SELECT sqrt(abs(numeric)) FROM table")
          |> Type.clear_column?(["sqrt"])
        )

    test "ignores aggregates", do: assert(type_first_column("SELECT max(numeric) FROM table") |> Type.clear_column?())

    test "does not ignore functions in aggregates",
      do: refute(type_first_column("SELECT max(sqrt(numeric)) FROM table") |> Type.clear_column?())
  end

  defp constant_involved?(query), do: type_first_column(query).constant_involved?

  defp constant?(query), do: type_first_column(query).constant?

  defp type_first_column(query) do
    compiled_query = compile!(query)
    Type.establish_type(hd(compiled_query.columns), compiled_query)
  end

  defp compile!(query_string) do
    {:ok, result, _features} = compile(query_string)
    result
  end

  defp compile(query_string), do: Compiler.compile(data_source(), Parser.parse!(query_string), [], %{})

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
              Table.column("numeric", :integer),
              Table.column("float", :real),
              Table.column("string", :text),
              Table.column("time", :time),
              Table.column("date", :date)
            ]
          )
      }
    }
  end
end
