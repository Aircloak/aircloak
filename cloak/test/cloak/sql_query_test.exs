defmodule Cloak.SqlQueryTest do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery
  alias Cloak.SqlQuery.Parsers.Token

  test "simple select query" do
    assert %{command: :select, columns: ["foo"], from: "baz"} == SqlQuery.parse!("select foo from baz")
  end

  test "fully qualified table name" do
    query = SqlQuery.parse!("select foo from bar.baz")
    assert %{command: :select, columns: ["foo"], from: "bar.baz"} == query
  end

  test "query with a terminating semicolon" do
    assert %{command: :select, columns: ["foo"], from: "baz"} == SqlQuery.parse!("select foo from baz;")
  end

  test "multiple fields" do
    query = SqlQuery.parse!("select foo, bar from baz")
    assert %{command: :select, columns: ["foo", "bar"], from: "baz"} == query
  end

  test "whitespaces are ignored" do
    query = SqlQuery.parse!("select  foo\n from \n \n baz \n ; \n  ")
    assert %{command: :select, columns: ["foo"], from: "baz"} == query
  end

  test "all allowed identifier characters" do
    query = SqlQuery.parse!("select foO1_ from Ba_z2")
    assert %{command: :select, columns: ["foO1_"], from: "Ba_z2"} == query
  end

  test "case insensivity of commands" do
    assert %{command: :select, columns: ["foo"], from: "baz"} == SqlQuery.parse!("SELECT foo FROM baz")
  end

  test "show tables" do
    assert %{command: :show, show: :tables} == SqlQuery.parse!("show tables")
  end

  test "show columns" do
    assert %{command: :show, show: :columns, from: "foo"} == SqlQuery.parse!("show columns from foo")
  end

  defmacrop verify_select(string, opts) do
    quote do
      assert %{unquote_splicing([command: :select] ++ opts)} = SqlQuery.parse!(unquote(string))
    end
  end

  defmacrop constant(value) do
    quote do
      %Token{category: :constant, value: %{value: unquote(value)}}
    end
  end

  defmacrop constants(values) do
    Enum.map(values, &quote(do: constant(unquote(&1))))
  end

  test "where clause with equality" do
    verify_select(
      "select foo from bar where a = 10",
      columns: ["foo"], from: "bar", where: [{:comparison, "a", :=, constant(10)}]
    )
  end

  test "where clause with <" do
    verify_select(
      "select foo from bar where a < 10",
      columns: ["foo"], from: "bar", where: [{:comparison, "a", :<, constant(10)}]
    )
  end

  test "where clause with >" do
    verify_select(
      "select foo from bar where a > 10",
      columns: ["foo"], from: "bar", where: [{:comparison, "a", :>, constant(10)}]
    )
  end

  test "where clause with >=" do
    verify_select(
      "select foo from bar where a >= 10",
      columns: ["foo"], from: "bar", where: [{:comparison, "a", :>=, constant(10)}]
    )
  end

  test "where clause with <=" do
    verify_select(
      "select foo from bar where a <= 10",
      columns: ["foo"], from: "bar", where: [{:comparison, "a", :<=, constant(10)}]
    )
  end

  test "where clause with <>" do
    verify_select(
      "select foo from bar where a <> 10",
      columns: ["foo"], from: "bar", where: [{:comparison, "a", :<>, constant(10)}]
    )
  end

  test "where clause can have float values" do
    verify_select(
      "select foo from bar where a = 10.0",
      columns: ["foo"], from: "bar", where: [{:comparison, "a", :=, constant(10.0)}]
    )
  end

  test "where clause can have string values" do
    verify_select(
      "select foo from bar where name = 'tom'",
      columns: ["foo"], from: "bar", where: [{:comparison, "name", :=, constant("tom")}]
    )
  end

  test "where clause can have string values of any case" do
    verify_select(
      "select foo from bar where name = 'tOm'",
      columns: ["foo"], from: "bar", where: [{:comparison, "name", :=, constant("tOm")}]
    )
  end

  test "where clause can have multi-word string values" do
    verify_select(
      "select foo from bar where name = 'avishai cohen'",
      columns: ["foo"], from: "bar", where: [{:comparison, "name", :=, constant("avishai cohen")}]
    )
  end

  test "where clause with mutliple comparisons" do
    verify_select(
      "select foo from bar where a <> 10 and b = 'bar'",
      columns: ["foo"], from: "bar",
      where: [{:comparison, "a", :<>, constant(10)}, {:comparison, "b", :=, constant("bar")}]
    )
  end

  test "where clause with LIKE is OK" do
    verify_select(
      "select foo from bar where a LIKE '_ob d%'",
      columns: ["foo"], from: "bar", where: [{:like, "a", constant("_ob d%")}]
    )
  end

  test "where clause with IN is OK" do
    verify_select(
      "select foo from bar where a IN (1, 2, 3)",
      columns: ["foo"], from: "bar", where: [{:in, "a", constants([1, 2, 3])}]
    )
  end

  test "where clause with all types of clauses is OK" do
    verify_select(
      "select foo from bar where a = 2 and b in (1,2,3) and c like '_o'",
      columns: ["foo"], from: "bar",
      where: [
        {:comparison, "a", :=, constant(2)},
        {:in, "b", constants([1, 2, 3])},
        {:like, "c", constant("_o")}
      ]
    )
  end

  test "boolean values are allowed in comparisons" do
    verify_select(
      "select foo from bar where a = true and b in (true, false)",
      columns: ["foo"], from: "bar",
      where: [{:comparison, "a", :=, constant(true)}, {:in, "b", constants([true, false])}]
    )
  end

  test "it's valid to have a identifier contain a keyword" do
    verify_select(
      "SELECT INvalid, selectiscious FROM whereables",
      columns: ["INvalid", "selectiscious"], from: "whereables"
    )
  end

  test "count(*)" do
    verify_select("select count(*) from foo", columns: [{:count, :star}], from: "foo")
  end

  test "group by multiple columns" do
    verify_select(
      "select x from b group by x, y, z",
      columns: ["x"], from: "b", group_by: ["x", "y", "z"]
    )
  end

  test "group by just one column" do
    verify_select("select a from b group by a", group_by: ["a"])
  end

  test "order by clause" do
    verify_select(
      "select a, b, c from foo order by a desc, b asc, c",
      columns: ["a", "b", "c"], from: "foo", order_by: [{"a", :desc}, {"b", :asc}, {"c", nil}]
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
