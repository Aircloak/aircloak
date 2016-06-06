defmodule Cloak.SqlQueryTest do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery
  alias Cloak.SqlQuery.Parsers.Token


  # -------------------------------------------------------------------
  # Helper macros
  # -------------------------------------------------------------------

  # These macros can be used to verify the AST produced by the parser. The
  # helpers are implemented as macros because we're testing the AST through
  # pattern matching, rather than comparison. This allows tests to be more
  # concise and skip specifying some details, such as line and column of
  # each element in the ast.

  # Runs the query string and asserts it matches to the given pattern.
  defmacrop assert_parse(query_string, expected_pattern) do
    quote do
      assert unquote(expected_pattern) = SqlQuery.parse!(unquote(query_string))
    end
  end

  # Produces a pattern which matches an AST of a select query.
  defmacrop select(select_data) do
    quote do
      %{unquote_splicing([command: :select] ++ select_data)}
    end
  end

  # Produces a pattern which matches an AST of a show query.
  defmacrop show(what, show_data \\ []) do
    quote do
      %{unquote_splicing([command: :show, show: what] ++ show_data)}
    end
  end

  # Produces a pattern which matches an AST of a constant.
  defmacrop constant(value) do
    quote do
      %Token{category: :constant, value: %{value: unquote(value)}}
    end
  end

  # Produces a pattern which matches an AST of multiple constants.
  defmacrop constants(values) do
    Enum.map(values, &quote(do: constant(unquote(&1))))
  end


  # -------------------------------------------------------------------
  # Tests
  # -------------------------------------------------------------------

  test "simple select query" do
    assert_parse("select foo from baz", select(columns: ["foo"], from: "baz"))
  end

  test "fully qualified table name" do
    assert_parse("select foo from bar.baz", select(columns: ["foo"], from: "bar.baz"))
  end

  test "query with a terminating semicolon" do
    assert_parse("select foo from baz;", select(columns: ["foo"], from: "baz"))
  end

  test "multiple fields" do
    assert_parse("select foo, bar from baz", select(columns: ["foo", "bar"], from: "baz"))
  end

  test "whitespaces are ignored" do
    assert_parse("select  foo\n from \n \n baz \n ; \n  ", select(columns: ["foo"], from: "baz"))
  end

  test "all allowed identifier characters" do
    assert_parse("select foO1_ from Ba_z2", select(columns: ["foO1_"], from: "Ba_z2"))
  end

  test "case insensivity of commands" do
    assert_parse("SELECT foo FROM baz", select(columns: ["foo"], from: "baz"))
  end

  test "show tables" do
    assert_parse("show tables", show(:tables))
  end

  test "show columns" do
    assert_parse("show columns from foo", show(:columns, from: "foo"))
  end

  test "where clause with equality" do
    assert_parse(
      "select foo from bar where a = 10",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "a", :=, constant(10)}])
    )
  end

  test "where clause with <" do
    assert_parse(
      "select foo from bar where a < 10",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "a", :<, constant(10)}])
    )
  end

  test "where clause with >" do
    assert_parse(
      "select foo from bar where a > 10",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "a", :>, constant(10)}])
    )
  end

  test "where clause with >=" do
    assert_parse(
      "select foo from bar where a >= 10",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "a", :>=, constant(10)}])
    )
  end

  test "where clause with <=" do
    assert_parse(
      "select foo from bar where a <= 10",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "a", :<=, constant(10)}])
    )
  end

  test "where clause with <>" do
    assert_parse(
      "select foo from bar where a <> 10",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "a", :<>, constant(10)}])
    )
  end

  test "where clause can have float values" do
    assert_parse(
      "select foo from bar where a = 10.0",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "a", :=, constant(10.0)}])
    )
  end

  test "where clause can have string values" do
    assert_parse(
      "select foo from bar where name = 'tom'",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "name", :=, constant("tom")}])
    )
  end

  test "where clause can have string values of any case" do
    assert_parse(
      "select foo from bar where name = 'tOm'",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "name", :=, constant("tOm")}])
    )
  end

  test "where clause can have multi-word string values" do
    assert_parse(
      "select foo from bar where name = 'avishai cohen'",
      select(columns: ["foo"], from: "bar", where: [{:comparison, "name", :=, constant("avishai cohen")}])
    )
  end

  test "where clause with mutliple comparisons" do
    assert_parse(
      "select foo from bar where a <> 10 and b = 'bar'",
      select(
        columns: ["foo"], from: "bar",
        where: [{:comparison, "a", :<>, constant(10)}, {:comparison, "b", :=, constant("bar")}]
      )
    )
  end

  test "where clause with LIKE is OK" do
    assert_parse(
      "select foo from bar where a LIKE '_ob d%'",
      select(columns: ["foo"], from: "bar", where: [{:like, "a", constant("_ob d%")}])
    )
  end

  test "where clause with NOT LIKE" do
    assert_parse(
      "select foo from bar where a NOT LIKE '%pattern%'",
      select(where: [{:not_like, "a", constant("%pattern%")}])
    )
  end

  test "where clause with IN is OK" do
    assert_parse(
      "select foo from bar where a IN (1, 2, 3)",
      select(columns: ["foo"], from: "bar", where: [{:in, "a", constants([1, 2, 3])}])
    )
  end

  test "where clause with all types of clauses is OK" do
    assert_parse(
      "select foo from bar where a = 2 and b in (1,2,3) and c like '_o'",
      select(
        columns: ["foo"], from: "bar",
        where: [
          {:comparison, "a", :=, constant(2)},
          {:in, "b", constants([1, 2, 3])},
          {:like, "c", constant("_o")}
        ]
      )
    )
  end

  test "boolean values are allowed in comparisons" do
    assert_parse(
      "select foo from bar where a = true and b in (true, false)",
      select(
        columns: ["foo"], from: "bar",
        where: [{:comparison, "a", :=, constant(true)}, {:in, "b", constants([true, false])}]
      )
    )
  end

  test "it's valid to have a identifier contain a keyword" do
    assert_parse(
      "SELECT INvalid, selectiscious FROM whereables",
      select(columns: ["INvalid", "selectiscious"], from: "whereables")
    )
  end

  test "count(*)" do
    assert_parse("select count(*) from foo", select(columns: [{:count, :star}], from: "foo"))
  end

  test "group by multiple columns" do
    assert_parse(
      "select x from b group by x, y, z",
      select(columns: ["x"], from: "b", group_by: ["x", "y", "z"])
    )
  end

  test "group by just one column" do
    assert_parse("select a from b group by a", select(group_by: ["a"]))
  end

  test "order by clause" do
    assert_parse(
      "select a, b, c from foo order by a desc, b asc, c",
      select(columns: ["a", "b", "c"], from: "foo", order_by: [{"a", :desc}, {"b", :asc}, {"c", nil}])
    )
  end

  Enum.each([
    {"single quote is not allowed in the identifier", "select fo'o from baz", "Invalid character"},
    {"identifier can't start with a number", "select 1foo from baz", "Expected `identifier`"},
    {"keyword is not identifier", "select select from baz", "Expected `identifier`"},
    {"from table is required", "select foo", "`from`"},
    {"at least one column must be specified", "select from baz", "Expected `identifier`"},
    {"columns must be separated with a comma", "select foo bar from baz", "Expected `from`"},
    {"query must start with a select or show", "foo select foo bar from baz", "Expected `select or show`"},
    {"show requires tables or columns", "show foobar", "Expected `tables or columns`"},
    {"!= is an illegal comparator in where clause", "select a from b where a != b", "Invalid character"},
    {"=> is an illegal comparator in where clause", "select a from b where a => b", "Expected `comparison value`"},
    {"=< is an illegal comparator in where clause", "select a from b where a =< b", "Expected `comparison value`"},
    {"where clauses cannot be separated by or", "select a from b where a > 1 or b < 2", "Expected end of input"},
    {"not joining multiple where clauses is illegal", "select a from b where a > 1 b < 2", "Expected end of input"},
    {"count requires parens", "select count * from foo", "Expected `(`"},
    {"cannot group by count", "select a from foo group by count(*)", "Expected `identifier`"},
    {"'by' has to follow 'order'", "select a from foo order a asc", "Expected `by`"},
    {"'by' has to follow 'group'", "select a from foo group a", "Expected `by`"},
    {"order by fields needs to be comma separated", "select a, b, c from foo order by a b", "Expected end of input"},
    {"invalid like", "select foo from bar where baz like", "Expected `string constant`"},
    {"invalid like type", "select foo from bar where baz like 10", "Expected `string constant`"},
    {"invalid in", "select foo from bar where baz in", "Expected `(`"},
    {"invalid comparison", "select foo from bar where baz =", "Expected `comparison value`"},
    {"missing where expression", "select foo from bar where", "Invalid where expression"},
    {"invalid where expression", "select foo from bar where foo bar", "Invalid where expression"},
    {"no input allowed after the statement", "select foo from bar baz", "Expected end of input"}
  ], fn {description, statement, expected_error} ->
    test description do
      assert {:error, reason} = SqlQuery.parse(unquote(statement))
      assert reason =~ unquote(expected_error)
    end
  end)
end
