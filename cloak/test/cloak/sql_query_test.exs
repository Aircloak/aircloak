defmodule Cloak.SqlQueryTest do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery

  test "simple select query" do
    assert %Cloak.SqlQuery{select: ["foo"], from: "baz"} == SqlQuery.parse!("select foo from baz")
  end

  test "fully qualified table name" do
    assert %Cloak.SqlQuery{select: ["foo"], from: "bar.baz"} == SqlQuery.parse!("select foo from bar.baz")
  end

  test "query with a terminating semicolon" do
    assert %Cloak.SqlQuery{select: ["foo"], from: "baz"} == SqlQuery.parse!("select foo from baz;")
  end

  test "multiple fields" do
    assert %Cloak.SqlQuery{select: ["foo", "bar"], from: "baz"} == SqlQuery.parse!("select foo, bar from baz")
  end

  test "whitespaces are ignored" do
    assert %Cloak.SqlQuery{select: ["foo"], from: "baz"} ==
      SqlQuery.parse!("select  foo\n from \n \n baz \n ; \n  ")
  end

  test "all allowed identifier characters" do
    assert %Cloak.SqlQuery{select: ["foO1_"], from: "Ba_z2"} == SqlQuery.parse!("select foO1_ from Ba_z2")
  end


  for {description, statement} <- [
    {"single quote is not allowed in the identifier", "select fo'o from baz"},
    {"identifier can't start with a number", "select 1foo from baz"},
    {"keyword is not identifier", "select select from baz"},
    {"from table is required", "select foo"},
    {"at least one column must be specified", "select from baz"},
    {"columns must be separated with a comma", "select foo bar from baz"},
    {"query must start with a select", "foo select foo bar from baz"}
  ] do
    test description do
      assert {:error, _} = SqlQuery.parse(unquote(statement))
    end
  end
end
