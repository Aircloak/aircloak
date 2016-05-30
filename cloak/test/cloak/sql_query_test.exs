defmodule Cloak.SqlQueryTest do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery

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

  test "where clause with equality" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:comparison, "a", :=, 10}]} ==
      SqlQuery.parse!("select foo from bar where a = 10")
  end

  test "where clause with <" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:comparison, "a", :<, 10}]} ==
      SqlQuery.parse!("select foo from bar where a < 10")
  end

  test "where clause with >" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:comparison, "a", :>, 10}]} ==
      SqlQuery.parse!("select foo from bar where a > 10")
  end

  test "where clause with >=" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:comparison, "a", :>=, 10}]} ==
      SqlQuery.parse!("select foo from bar where a >= 10")
  end

  test "where clause with <=" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:comparison, "a", :<=, 10}]} ==
      SqlQuery.parse!("select foo from bar where a <= 10")
  end

  test "where clause with <>" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:comparison, "a", :<>, 10}]} ==
      SqlQuery.parse!("select foo from bar where a <> 10")
  end

  test "where clause can have string values" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:comparison, "name", :=, "'tom'"}]} ==
      SqlQuery.parse!("select foo from bar where name = 'tom'")
  end

  test "where clause can have string values of any case" do
    assert %{command: :select, columns: ["foo"], from: "bar", where:
        [{:comparison, "name", :=, "'tOm'"}]} ==
      SqlQuery.parse!("select foo from bar where name = 'tOm'")
  end

  test "where clause can have multi-word string values" do
    assert %{command: :select, columns: ["foo"], from: "bar", where:
        [{:comparison, "name", :=, "'avishai cohen'"}]} ==
      SqlQuery.parse!("select foo from bar where name = 'avishai cohen'")
  end

  test "where clause with mutliple comparisons" do
    assert %{command: :select, columns: ["foo"], from: "bar", where:
      [{:comparison, "a", :<>, 10}, {:comparison, "b", :=, "'bar'"}]} ==
      SqlQuery.parse!("select foo from bar where a <> 10 and b = 'bar'")
  end

  test "where clause with LIKE is OK" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:like, "a", "'_ob d%'"}]} ==
      SqlQuery.parse!("select foo from bar where a LIKE '_ob d%'")
  end

  test "where clause with IN is OK" do
    assert %{command: :select, columns: ["foo"], from: "bar", where: [{:in, "a", [1, 2, 3]}]} ==
      SqlQuery.parse!("select foo from bar where a IN (1, 2, 3)")
  end

  test "where clause with all types of clauses is OK" do
    assert %{command: :select, columns: ["foo"], from: "bar", where:
        [{:comparison, "a", :=, 2}, {:in, "b", [1, 2, 3]}, {:like, "c", "'_o'"}]} ==
      SqlQuery.parse!("select foo from bar where a = 2 and b in (1,2,3) and c like '_o'")
  end

  for {description, statement, expected_error} <- [
    {"single quote is not allowed in the identifier", "select fo'o from baz", "Expected `from`"},
    {"identifier can't start with a number", "select 1foo from baz", "Expected `identifier`"},
    {"keyword is not identifier", "select select from baz", "Expected `identifier`"},
    {"from table is required", "select foo", "`from`"},
    {"at least one column must be specified", "select from baz", "Expected `identifier`"},
    {"columns must be separated with a comma", "select foo bar from baz", "Expected `from`"},
    {"query must start with a select or show", "foo select foo bar from baz", "Expected `select or show`"},
    {"show requires tables or columns", "show foobar", "Expected `tables or columns`"},
    {"!= is an illegal comparator in where clause", "select a from b where a != b"},
    {"=> is an illegal comparator in where clause", "select a from b where a => b"},
    {"=< is an illegal comparator in where clause", "select a from b where a =< b"},
    {"multiple where clauses cannot be separated by or", "select a from b where a > 1 or b < 2"},
    {"not joining multiple where clauses is illegal", "select a from b where a > 1 b < 2"}
  ] do
    test description do
      assert {:error, reason} = SqlQuery.parse(unquote(statement))
      assert reason =~ unquote(expected_error)
    end
  end
end
