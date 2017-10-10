defmodule Cloak.Sql.TypeChecker.Test do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser, TypeChecker, Expression}

  describe "math is only dangerous if a constant is involved" do
    Enum.each(~w(+ - * ^ /), fn(math_function) ->
      test "#{math_function} with no constant" do
        query = "SELECT numeric #{unquote(math_function)} numeric FROM table"
        refute seen_dangerous_math?(query)
      end
    end)

    # Note: we can't test with / because it is treated as a dangerous discontinuous
    # function as soon as a constant is involved, and hence halts query compilation.
    Enum.each(~w(+ - * ^), fn(math_function) ->
      test "#{math_function} with constant" do
        query = "SELECT numeric #{unquote(math_function)} 2 FROM table"
        assert seen_dangerous_math?(query)
      end
    end)

    test "pow with no constant" do
      query = "SELECT pow(numeric, numeric) FROM table"
      refute seen_dangerous_math?(query)
    end

    test "pow with constant" do
      query = "SELECT pow(numeric, 2) FROM table"
      assert seen_dangerous_math?(query)
    end
  end

  describe "Discontinuous functions not dangerous when no constant" do
    Enum.each(~w(abs ceil floor round trunc sqrt), fn(discontinuous_function) ->
      test discontinuous_function do
        query = "SELECT #{unquote(discontinuous_function)}(numeric) FROM table"
        refute dangerously_discontinuous?(query)
      end
    end)

    Enum.each(~w(div mod), fn(discontinuous_function) ->
      test discontinuous_function do
        query = "SELECT #{unquote(discontinuous_function)}(numeric, numeric) FROM table"
        refute dangerously_discontinuous?(query)
      end
    end)

    # Note bucket isn't tested here, because it only compiles if the alignment value is a constant.
  end

  describe "casts are not dangerously discontinuous by themselves" do
    Enum.each(~w(integer real boolean), fn(cast_target) ->
      test "cast from integer to #{cast_target}" do
        query = "SELECT cast(numeric as #{unquote(cast_target)}) FROM table"
        refute dangerously_discontinuous?(query)
      end
    end)

    Enum.each(~w(datetime time date text interval), fn(cast_target) ->
      test "cast from text to #{cast_target}" do
        query = "SELECT cast(string as #{unquote(cast_target)}) FROM table"
        refute dangerously_discontinuous?(query)
      end
    end)
  end

  describe "discontinuous string functions only dangerous if later converted to a number" do
    Enum.each(~w(btrim ltrim rtrim), fn(discontinuous_function) ->
      test "first #{discontinuous_function}, but not cast to number" do
        query = "SELECT #{unquote(discontinuous_function)}(string, 'trim') FROM table"
        refute dangerously_discontinuous?(query)
      end

      Enum.each(~w(integer real boolean), fn(cast_target) ->
        test "first #{discontinuous_function}, then cast to #{cast_target}" do
          query = """
          SELECT cast(#{unquote(discontinuous_function)}(string, 'trim') as #{unquote(cast_target)})
          FROM table
          """
          assert dangerously_discontinuous?(query)
        end
      end)

      test "first #{discontinuous_function}, then use of length" do
        query = "SELECT length(#{unquote(discontinuous_function)}(string, 'trim')) FROM table"
        assert dangerously_discontinuous?(query)
      end
    end)

    Enum.each(~w(left right), fn(discontinuous_function) ->
      test "first #{discontinuous_function}, but not cast to number" do
        query = "SELECT #{unquote(discontinuous_function)}(string, 2) FROM table"
        refute dangerously_discontinuous?(query)
      end

      Enum.each(~w(integer real boolean), fn(cast_target) ->
        test "first #{discontinuous_function}, then cast to #{cast_target}" do
          query = """
          SELECT cast(#{unquote(discontinuous_function)}(string, 2) as #{unquote(cast_target)})
          FROM table
          """
          assert dangerously_discontinuous?(query)
        end
      end)

      test "first #{discontinuous_function}, then use of length" do
        query = "SELECT length(#{unquote(discontinuous_function)}(string, 2)) FROM table"
        assert dangerously_discontinuous?(query)
      end
    end)

    test "first substring, but not cast to number" do
      query = "SELECT substring(string from 2) FROM table"
      refute dangerously_discontinuous?(query)
    end

    Enum.each(~w(integer real boolean), fn(cast_target) ->
      test "first substring, then cast to #{cast_target}" do
        query = """
        SELECT cast(substring(string from 2) as #{unquote(cast_target)})
        FROM table
        """
        assert dangerously_discontinuous?(query)
      end
    end)

    test "first substring, then use of length" do
      query = "SELECT length(substring(string from 2)) FROM table"
      assert dangerously_discontinuous?(query)
    end
  end

  def expression_name(breadcrumb_trail), do:
    breadcrumb_trail
    |> Enum.map(fn({expression, _}) -> expression end)
    |> Enum.map(&(&1.name))

  describe "knows which columns were involved" do
    test "when a column is selected" do
      type = type_first_column("SELECT numeric FROM table")
      assert expression_name(type.narrative_breadcrumbs) == ["numeric"]
    end

    test "when a column is selected inside a function" do
      type = type_first_column("SELECT abs(numeric) FROM table")
      assert expression_name(type.narrative_breadcrumbs) == ["numeric"]
    end

    test "when a column is selected in a subquery" do
      type = type_first_column("SELECT col FROM (SELECT uid, numeric as col FROM table) t")
      assert expression_name(type.narrative_breadcrumbs) == ["numeric"]
    end

    test "when multiple columns are selected" do
      type = type_first_column("SELECT concat(string, cast(numeric as text)) FROM table")
      assert expression_name(type.narrative_breadcrumbs) == ["string", "numeric"]
    end
  end

  describe "detection of datetime functions" do
    Enum.each(~w(year quarter month day hour minute second weekday), fn(datetime_function) ->
      test "#{datetime_function} triggers datetime function recognition on a datetime column" do
        type = type_first_column("SELECT #{unquote(datetime_function)}(column) FROM table")
        assert type.is_result_of_datetime_processing?
      end
    end)

    Enum.each(~w(hour minute second), fn(datetime_function) ->
      test "#{datetime_function} triggers datetime function recognition on a time column" do
        type = type_first_column("SELECT #{unquote(datetime_function)}(time) FROM table")
        assert type.is_result_of_datetime_processing?
      end
    end)

    Enum.each(~w(year quarter month day weekday), fn(datetime_function) ->
      test "#{datetime_function} triggers datetime function recognition on a date column" do
        type = type_first_column("SELECT #{unquote(datetime_function)}(date) FROM table")
        assert type.is_result_of_datetime_processing?
      end
    end)

    test "does not triggers datetime function recognition when none is used" do
      type = type_first_column("SELECT column FROM table")
      refute type.is_result_of_datetime_processing?
    end
  end

  describe "records a trail of narrative breadcrumbs" do
    test "empty narrative for queries without functions or math" do
      type = type_first_column("SELECT numeric FROM table")
      assert [{%Expression{name: "numeric"}, []}] = type.narrative_breadcrumbs
    end

    test "for function with discontinuious function div" do
      type = type_first_column("SELECT div(numeric, 10) FROM table")
      assert [{%Expression{name: "numeric"}, [{:dangerously_discontinuous, "div"}]}] = type.narrative_breadcrumbs
    end

    test "even when multiple occur" do
      type = type_first_column("SELECT abs(div(numeric, 10)) FROM table")
      [{%Expression{name: "numeric"}, [
         {:dangerously_discontinuous, "abs"},
         {:dangerously_discontinuous, "div"}]
      }] = type.narrative_breadcrumbs
    end

    test "does not record discontinuous functions that aren't dangerous" do
      type = type_first_column("SELECT div(cast(sqrt(numeric) as integer), 10) FROM table")
      [{%Expression{name: "numeric"}, [{:dangerously_discontinuous, "div"}]}] = type.narrative_breadcrumbs
    end

    test "records math influenced by a constant as a potential offense" do
      type = type_first_column("SELECT numeric + 10 FROM table")
      [{%Expression{name: "numeric"}, [{:dangerous_math, "+"}]}] = type.narrative_breadcrumbs
    end

    test "does not record math between non-constant influenced columns" do
      type = type_first_column("SELECT numeric + numeric FROM table")
      assert [{_column_expressions, []}] = type.narrative_breadcrumbs
    end

    test "records usage of datetime functions as a potential offense" do
      type = type_first_column("SELECT year(column) FROM table")
      [{%Expression{name: "column"}, [{:datetime_processing, "year"}]}] = type.narrative_breadcrumbs
    end

    test "records casts of datetime's as a potential offense" do
      type = type_first_column("SELECT cast(column as text) FROM table")
      [{%Expression{name: "column"}, [{:datetime_processing, {:cast, :text}}]}] = type.narrative_breadcrumbs
    end

    test "records multiple math offenses" do
      type = type_first_column("SELECT numeric + 10 FROM (SELECT uid, numeric - 1 as numeric FROM table) t")
      assert [{%Expression{name: "numeric"}, [{:dangerous_math, "+"}, {:dangerous_math, "-"}]}] =
        type.narrative_breadcrumbs
    end

    test "deduplicates offenses - math" do
      type = type_first_column("SELECT numeric + 10 + 10 FROM table")
      assert [{%Expression{name: "numeric"}, [{:dangerous_math, "+"}]}] = type.narrative_breadcrumbs
    end

    test "deduplicates offenses - discontinuity" do
      type = type_first_column("SELECT numeric % 2 % 2 FROM table")
      assert [{%Expression{name: "numeric"}, [{:dangerously_discontinuous, "%"}]}] =
        type.narrative_breadcrumbs
    end
  end

  defp dangerously_discontinuous?(query), do:
    type_first_column(query).dangerously_discontinuous?

  defp seen_dangerous_math?(query), do:
    type_first_column(query).seen_dangerous_math?

  defp type_first_column(query) do
    compiled_query = compile!(query)
    TypeChecker.establish_type(hd(compiled_query.columns), compiled_query)
  end

  defp compile!(query_string) do
    {:ok, result, _features} = compile(query_string)
    result
  end

  defp compile(query_string), do:
    Compiler.compile(data_source(), Parser.parse!(query_string), [], %{})

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
            Table.column("string", :text),
            Table.column("time", :time),
            Table.column("date", :date),
          ]
        )
      }
    }
  end
end
