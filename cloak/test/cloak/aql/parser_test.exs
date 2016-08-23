defmodule Cloak.Aql.Parser.Test do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Parser

  @psql_data_source %{driver: Cloak.DataSource.PostgreSQL}
  @ds_proxy_data_source %{driver: Cloak.DataSource.DsProxy}


  # -------------------------------------------------------------------
  # Helper macros
  # -------------------------------------------------------------------

  # These macros can be used to verify the AST produced by the parser. The
  # helpers are implemented as macros because we're testing the AST through
  # pattern matching, rather than comparison. This allows tests to be more
  # concise and skip specifying some details, such as line and column of
  # each element in the ast.

  # Runs the query string and asserts it matches to the given pattern.
  defmacrop assert_parse(query_string, expected_pattern, data_source \\ quote(do: @psql_data_source)) do
    quote do
      assert unquote(expected_pattern) = Parser.parse!(unquote(data_source), unquote(query_string))
    end
  end

  defmacrop assert_parse_error(query_string, expected_pattern, data_source \\ quote(do: @psql_data_source)) do
    quote do
      assert {:error, unquote(expected_pattern)} = Parser.parse(unquote(data_source), unquote(query_string))
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
      {:constant, _, value}
    end
  end

  # Produces a pattern which matches an AST of a constant with type
  defmacrop constant(type, value) do
    quote do
      {:constant, type, value}
    end
  end

  # Produces a pattern which matches an AST of multiple constants.
  defmacrop constants(values) do
    Enum.map(values, &quote(do: constant(unquote(&1))))
  end

  defmacrop parsed_subquery(value, alias) do
    quote do
      {:subquery, %{type: :parsed, ast: unquote(value), alias: unquote(alias)}}
    end
  end

  defmacrop unparsed_subquery(value) do
    quote do
      {:subquery, %{type: :unparsed, unparsed_string: unquote(value)}}
    end
  end

  # Produces a pattern which matches an identifier with the given name.
  defmacrop identifier(name) do
    quote do
      {:identifier, :unknown, name}
    end
  end

  defmacrop cross_join(lhs, rhs) do
    quote do
      join(:cross_join, unquote(lhs), unquote(rhs), [])
    end
  end

  for join_type <- [:inner_join, :full_outer_join, :left_outer_join, :right_outer_join] do
    defmacrop unquote(join_type)(lhs, rhs, conditions) do
      join_type = unquote(join_type)
      quote do
        join(unquote(join_type), unquote(lhs), unquote(rhs), unquote(conditions))
      end
    end
  end

  defmacrop join(join_type, lhs, rhs, conditions) do
    quote do
      {
        :join,
        %{type: unquote(join_type), lhs: unquote(lhs), rhs: unquote(rhs), conditions: unquote(conditions)}
      }
    end
  end


  # -------------------------------------------------------------------
  # Tests
  # -------------------------------------------------------------------

  test "simple select query" do
    assert_parse("select foo from baz", select(columns: [{:identifier, :unknown, "foo"}], from: "baz"))
  end

  test "fully qualified table name" do
    assert_parse("select foo from bar.baz",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar.baz"))
  end

  test "query with a terminating semicolon" do
    assert_parse("select foo from baz;",
      select(columns: [{:identifier, :unknown, "foo"}], from: "baz"))
  end

  test "multiple fields" do
    assert_parse("select foo, bar from baz",
      select(columns: [{:identifier, :unknown, "foo"}, {:identifier, :unknown, "bar"}], from: "baz"))
  end

  test "all fields" do
    assert_parse("select * from baz", select(columns: :*, from: "baz"))
  end

  test "whitespaces are ignored" do
    assert_parse("select  foo\n from \n \n baz \n ; \n  ",
      select(columns: [{:identifier, :unknown, "foo"}], from: "baz"))
  end

  test "all allowed identifier characters" do
    assert_parse("select foO1_ from Ba_z2",
      select(columns: [{:identifier, :unknown, "foO1_"}], from: "Ba_z2"))
  end

  test "case insensivity of commands" do
    assert_parse("SELECT foo FROM baz", select(columns: [{:identifier, :unknown, "foo"}], from: "baz"))
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
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :=, constant(10)}])
    )
  end

  test "where clause with <" do
    assert_parse(
      "select foo from bar where a < 10",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :<, constant(10)}])
    )
  end

  test "where clause with >" do
    assert_parse(
      "select foo from bar where a > 10",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :>, constant(10)}])
    )
  end

  test "where clause with >=" do
    assert_parse(
      "select foo from bar where a >= 10",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :>=, constant(10)}])
    )
  end

  test "where clause with <=" do
    assert_parse(
      "select foo from bar where a <= 10",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :<=, constant(10)}])
    )
  end

  test "where clause with <>" do
    assert_parse(
      "select foo from bar where a <> 10",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:not, {:comparison, {:identifier, :unknown, "a"}, :=, constant(10)}}])
    )
  end

  test "where clause can have float values" do
    assert_parse(
      "select foo from bar where a = 10.0",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :=, constant(10.0)}])
    )
  end

  test "where clause can have string values" do
    assert_parse(
      "select foo from bar where name = 'tom'",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "name"}, :=, constant("tom")}])
    )
  end

  test "where clause comparing two columns" do
    assert_parse(
      "select foo from bar where a = b",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}])
    )
  end

  test "where clause can have string values of any case" do
    assert_parse(
      "select foo from bar where name = 'tOm'",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "name"}, :=, constant("tOm")}])
    )
  end

  test "where clause can have multi-word string values" do
    assert_parse(
      "select foo from bar where name = 'avishai cohen'",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "name"}, :=, constant("avishai cohen")}])
    )
  end

  test "where clause with mutliple comparisons" do
    assert_parse(
      "select foo from bar where a <> 10 and b = 'bar'",
      select(
        columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [
          {:not, {:comparison, {:identifier, :unknown, "a"}, :=, constant(10)}},
          {:comparison, {:identifier, :unknown, "b"}, :=, constant("bar")}
        ]
      )
    )
  end

  test "where clause with BETWEEN" do
    assert_parse(
      "select foo from bar where a between 10 and 20",
      select(
        columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [
          {:comparison, {:identifier, :unknown, "a"}, :>=, constant(10)},
          {:comparison, {:identifier, :unknown, "a"}, :<=, constant(20)}
        ]
      )
    )
  end

  test "where clause with LIKE" do
    assert_parse(
      "select foo from bar where a LIKE '_ob d%'",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:like, {:identifier, :unknown, "a"}, constant("_ob d%")}])
    )
  end

  test "where clause with NOT LIKE" do
    assert_parse(
      "select foo from bar where a NOT LIKE '%pattern%'",
      select(where: [{:not, {:like, {:identifier, :unknown, "a"}, constant("%pattern%")}}])
    )
  end

  test "where clause with ILIKE" do
    assert_parse(
      "select foo from bar where a ILIKE '_ob d%'",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:ilike, {:identifier, :unknown, "a"}, constant("_ob d%")}])
    )
  end

  test "where clause with NOT ILIKE" do
    assert_parse(
      "select foo from bar where a NOT ILIKE '%pattern%'",
      select(where: [{:not, {:ilike, {:identifier, :unknown, "a"}, constant("%pattern%")}}])
    )
  end

  test "where clause with IN" do
    assert_parse(
      "select foo from bar where a IN (1, 2, 3)",
      select(columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:in, {:identifier, :unknown, "a"}, constants([1, 2, 3])}])
    )
  end

  test "where clause with IS and IS NOT" do
    assert_parse(
      "select foo from bar where a is null and b is not null",
      select(where: [{:is, {:identifier, :unknown, "a"}, :null},
        {:not, {:is, {:identifier, :unknown, "b"}, :null}}])
    )
  end

  test "where clause with all types of clauses" do
    assert_parse(
      "select foo from bar where a = 2 and b in (1,2,3) and c like '_o' and d is not null",
      select(
        columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [
          {:comparison, {:identifier, :unknown, "a"}, :=, constant(2)},
          {:in, {:identifier, :unknown, "b"}, constants([1, 2, 3])},
          {:like, {:identifier, :unknown, "c"}, constant("_o")},
          {:not, {:is, {:identifier, :unknown, "d"}, :null}},
        ]
      )
    )
  end

  test "boolean values are allowed in comparisons" do
    assert_parse(
      "select foo from bar where a = true and b in (true, false)",
      select(
        columns: [{:identifier, :unknown, "foo"}], from: "bar",
        where: [{:comparison, {:identifier, :unknown, "a"}, :=, constant(true)},
          {:in, {:identifier, :unknown, "b"}, constants([true, false])}]
      )
    )
  end

  test "it's valid to have a identifier contain a keyword" do
    assert_parse(
      "SELECT INvalid, selectiscious FROM whereables",
      select(columns: [{:identifier, :unknown, "INvalid"},
        {:identifier, :unknown, "selectiscious"}], from: "whereables")
    )
  end

  test "date extraction" do
    assert_parse(
      "SELECT extract(year FROM column) FROM table",
      select(columns: [{:function, "year", {:identifier, :unknown, "column"}}])
    )
  end

  test "count(*)" do
    assert_parse("select count(*) from foo", select(columns: [{:function, "count", [:*]}], from: "foo"))
  end

  test "aggregation functions" do
    assert_parse(
      "select sum(price), min(value) from foo",
      select(columns: [{:function, "sum", [{:identifier, :unknown, "price"}]},
        {:function, "min", [{:identifier, :unknown, "value"}]}], from: "foo")
    )
  end

  test "group by multiple columns" do
    assert_parse(
      "select x from b group by x, y, z",
      select(columns: [{:identifier, :unknown, "x"}], from: "b",
        group_by: [{:identifier, :unknown, "x"}, {:identifier, :unknown, "y"}, {:identifier, :unknown, "z"}])
    )
  end

  test "group by just one column" do
    assert_parse("select a from b group by a", select(group_by: [{:identifier, :unknown, "a"}]))
  end

  test "order by clause" do
    assert_parse(
      "select a, b, c from foo order by a desc, b asc, c",
      select(columns: [
          {:identifier, :unknown, "a"}, {:identifier, :unknown, "b"}, {:identifier, :unknown, "c"}
        ], from: "foo", order_by: [
          {{:identifier, :unknown, "a"}, :desc}, {{:identifier, :unknown, "b"}, :asc},
          {{:identifier, :unknown, "c"}, nil}
        ])
    )
  end

  test "parsed subquery sql" do
    assert_parse(
      "select foo from (select foo from bar) alias",
      select(columns: [{:identifier, :unknown, "foo"}], from: parsed_subquery(subquery, "alias"))
    )
    assert select(columns: [{:identifier, :unknown, "foo"}], from: "bar") = subquery
  end

  test "parsed nested subquery sql" do
    assert_parse(
      "select foo from (select foo from (select foo from bar) inner_alias) outer_alias",
      select(columns: [{:identifier, :unknown, "foo"}], from: parsed_subquery(subquery, "outer_alias"))
    )

    assert select(
      columns: [{:identifier, :unknown, "foo"}],
      from: parsed_subquery(inner_subquery, "inner_alias")
    ) = subquery

    assert select(columns: [{:identifier, :unknown, "foo"}], from: "bar") = inner_subquery
  end

  test "join of parsed subqueries" do
    assert_parse(
      "select foo from (select foo from bar) sq1, (select foo from baz) sq2",
      select(
        columns: [{:identifier, :unknown, "foo"}],
        from: cross_join(parsed_subquery(sq1, "sq1"), parsed_subquery(sq2, "sq2"))
      )
    )
    assert select(columns: [{:identifier, :unknown, "foo"}], from: "bar") = sq1
    assert select(columns: [{:identifier, :unknown, "foo"}], from: "baz") = sq2
  end

  test "joining table with a parsed subquery" do
    assert_parse(
      "select foo from bar inner join (select foo from baz) sq on bar.id = sq.id",
      select(
        columns: [{:identifier, :unknown, "foo"}],
        from: inner_join("bar", parsed_subquery(sq, "sq"), _comparison)
      )
    )
    assert select(columns: [{:identifier, :unknown, "foo"}], from: "baz") = sq
  end

  test "unparsed subquery sql" do
    assert_parse(
      "select foo from (select bar from baz) alias",
      select(columns: [{:identifier, :unknown, "foo"}], from: unparsed_subquery("select bar from baz")),
      @ds_proxy_data_source
    )
  end

  test "unparsed subquery sql with AS" do
    assert_parse(
      "select foo from (select bar from baz) as alias",
      select(columns: [{:identifier, :unknown, "foo"}], from: unparsed_subquery("select bar from baz")),
      @ds_proxy_data_source
    )
  end

  test "subquery sql with semicolon" do
    assert_parse(
      "select foo from (select bar from baz) alias;",
      select(columns: [{:identifier, :unknown, "foo"}], from: unparsed_subquery("select bar from baz")),
      @ds_proxy_data_source
    )
  end

  test "unparsed subquery sql with an unknown token" do
    assert_parse(
      "select foo from (select `bar` from baz) alias",
      select(columns: [{:identifier, :unknown, "foo"}], from: unparsed_subquery("select `bar` from baz")),
      @ds_proxy_data_source
    )
  end

  test "unparsed subquery sql with parens" do
    assert_parse(
      "select foo from (select bar, cast(now() as text) from baz) alias",
      select(
        columns: [{:identifier, :unknown, "foo"}],
        from: unparsed_subquery("select bar, cast(now() as text) from baz")
      ),
      @ds_proxy_data_source
    )
  end

  test "unparsed subquery sql with newlines" do
    assert_parse(
      "select foo from (select bar\n, \n\ncast(now()\n as text) from baz\n) alias",
      select(columns: [{:identifier, :unknown, "foo"}],
        from: unparsed_subquery("select bar\n, \n\ncast(now()\n as text) from baz\n")),
      @ds_proxy_data_source
    )
  end

  test "count(distinct column)" do
    assert_parse(
      "select count(distinct foo) from bar",
      select(columns: [{:function, "count", [{:distinct, {:identifier, :unknown, "foo"}}]}])
    )
  end

  test "fully qualified identifiers" do
    assert_parse(
      """
      SELECT table.column, count(table.column), count(distinct table.column)
      FROM table
      WHERE table.column = 't'
      GROUP BY table.column
      ORDER BY table.column DESC, count(distinct table.column)
      """,
      select(
        columns: [
          {:identifier, "table", "column"},
          {:function, "count", [{:identifier, "table", "column"}]},
          {:function, "count", [{:distinct, {:identifier, "table", "column"}}]}
        ],
        where: [{:comparison, {:identifier, "table", "column"}, :=, _}],
        group_by: [{:identifier, "table", "column"}],
        order_by: [
          {{:identifier, "table", "column"}, :desc},
          {{:function, "count", [{:distinct, {:identifier, "table", "column"}}]}, :nil}
        ]
      )
    )
  end

  test "allow selection of multiple tables" do
    assert_parse("select a from foo, bar, baz",
      select(columns: [{:identifier, :unknown, "a"}],
        from: cross_join("foo", cross_join("bar", "baz"))))
  end

  test "allow CROSS JOINs" do
    assert_parse("select a from foo CROSS JOIN bar",
      select(columns: [{:identifier, :unknown, "a"}], from: cross_join("foo", "bar")))
  end

  test "allow multiple CROSS JOINs" do
    assert_parse("select a from t1 CROSS JOIN t2 CROSS JOIN t3 CROSS JOIN t4",
      select(columns: [{:identifier, :unknown, "a"}], from:
        cross_join(cross_join(cross_join("t1", "t2"), "t3"), "t4")
      ))
  end

  test "allow INNER JOINs" do
    assert_parse("select a from foo JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: inner_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
    assert_parse("select a from foo INNER JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: inner_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
  end

  test "allow multiple INNER JOINs" do
    assert_parse("select a from foo JOIN bar ON a = b INNER JOIN baz ON b = c",
      select(columns: [{:identifier, :unknown, "a"}],
        from: inner_join(
            inner_join("foo", "bar", [
            {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
          ]), "baz", [
          {:comparison, {:identifier, :unknown, "b"}, :=, {:identifier, :unknown, "c"}}
        ])))
  end

  test "allow LEFT JOINs" do
    assert_parse("select a from foo LEFT JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: left_outer_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
    assert_parse("select a from foo LEFT OUTER JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: left_outer_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
  end

  test "allow RIGHT JOINs" do
    assert_parse("select a from foo RIGHT JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: right_outer_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
    assert_parse("select a from foo RIGHT OUTER JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: right_outer_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
  end

  test "allow FULL OUTER JOINs" do
    assert_parse("select a from foo FULL JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: full_outer_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
    assert_parse("select a from foo FULL OUTER JOIN bar ON a = b",
      select(columns: [{:identifier, :unknown, "a"}],
        from: full_outer_join("foo", "bar", [
          {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
        ])))
  end

  test "allow combining JOIN types" do
    assert_parse("select a from t1, t2 JOIN t3 ON a = b CROSS JOIN t4, t5 CROSS JOIN t6",
      select(columns: [{:identifier, :unknown, "a"}],
        from: cross_join("t1",
                cross_join(
                  cross_join(
                    inner_join("t2", "t3", [
                      {:comparison, {:identifier, :unknown, "a"}, :=, {:identifier, :unknown, "b"}}
                    ]),
                    "t4"
                  ),
                  cross_join("t5", "t6")))))
  end

  Enum.each(["JOIN", "INNER JOIN", "RIGHT JOIN", "RIGHT OUTER JOIN", "LEFT JOIN", "LEFT OUTER JOIN",
      "FULL JOIN", "FULL OUTER JOIN"],
    fn(join_type) ->
      test "Fails when no ON clause is provided in complex JOIN (#{join_type})" do
        assert_parse_error("select a from foo #{unquote(join_type)} bar", "Expected `on`" <> _)
      end
    end
  )

  test "column alias" do
    assert_parse("select a as x from b",
      select(columns: [{{:identifier, :unknown, "a"}, :as, "x"}]))
  end

  test "function in group by" do
    assert_parse("select * from foo group by bar(x)",
      select(group_by: [{:function, "bar", [{:identifier, :unknown, "x"}]}])
    )
  end

  test "select a constant" do
    assert_parse("select 10 from foo", select(columns: [constant(:integer, 10)]))
  end

  test "multi-argument function" do
    assert_parse("select foo(x, y, z) from bar", select(columns:
      [{:function, "foo", [identifier("x"), identifier("y"), identifier("z")]}]
    ))
  end

  test "extended btrim" do
    assert_parse "select trim(both from foo) from bar",
      select(columns: [{:function, "btrim", [identifier("foo")]}])
  end

  test "extended ltrim" do
    assert_parse "select trim(leading from foo) from bar",
      select(columns: [{:function, "ltrim", [identifier("foo")]}])
  end

  test "extended rtrim" do
    assert_parse "select trim(trailing from foo) from bar",
      select(columns: [{:function, "rtrim", [identifier("foo")]}])
  end

  test "extended with character set" do
    assert_parse "select trim(both 'xyz' from foo) from bar",
      select(columns: [{:function, "btrim", [identifier("foo"), constant(:text, "xyz")]}])
  end

  test "substring from" do
    assert_parse "select substring(foo from 3) from bar",
      select(columns: [{:function, "substring", [identifier("foo"), constant(:integer, 3)]}])
  end

  test "substring from ... for ..." do
    assert_parse "select substring(foo from 3 for 10) from bar",
      select(columns: [{:function, "substring", [
        identifier("foo"), constant(:integer, 3), constant(:integer, 10)]}])
  end

  test "substring for" do
    assert_parse "select substring(foo for 3) from bar",
      select(columns: [{:function, "substring_for", [identifier("foor"), constant(:integer, 3)]}])
  end

  test "||" do
    assert_parse "select a || b || c from bar",
      select(columns: [{:function, "||", [
        {:function, "||", [identifier("a"), identifier("b")]},
        identifier("c")]}])
  end

  test "+ and -" do
    assert_parse "select a + b - c + d from bar",
      select(columns: [{:function, "+", [
        {:function, "-", [
          {:function, "+", [identifier("a"), identifier("b")]},
          identifier("c")]},
        identifier("d")]}])
  end

  test "*, /, and %" do
    assert_parse "select a * b / c % d from bar",
      select(columns: [{:function, "%", [
        {:function, "/", [
          {:function, "*", [identifier("a"), identifier("b")]},
          identifier("c")]},
        identifier("d")]}])
  end

  test "^" do
    assert_parse "select a ^ b ^ c from bar",
      select(columns: [{:function, "^", [
        {:function, "^", [identifier("b"), identifier("c")]},
        identifier("a")]}])
  end

  test "()" do
    assert_parse "select a ^ ((b + c) * (d - e)) from foo",
      select(columns: [{:function, "^", [
        identifier("a"),
        {:function, "*", [
          {:function, "+", [identifier("b"), identifier("c")]},
          {:function, "-", [identifier("d"), identifier("e")]}]}]}])
  end

  test "* and / have higher precedence than + and -" do
    assert_parse "select a * b + c / d - e from bar",
      select(columns: [{:function, "-", [
        {:function, "+", [
          {:function, "*", [identifier("a"), identifier("b")]},
          {:function, "/", [identifier("c"), identifier("d")]}]},
        identifier("e")]}])
  end

  test "^ has a higher precedence than *" do
    assert_parse "select a ^ b * c from bar",
      select(columns: [{:function, "*", [
        {:function, "^", [identifier("a"), identifier("b")]},
        identifier("c")]}])
  end

  test "cast" do
    assert_parse "select cast(a, integer) from bar",
      select(columns: [{:function, {:cast, :integer}, [identifier("a")]}])
  end

  test "extended cast" do
    assert_parse "select cast(a as text) from bar",
      select(columns: [{:function, {:cast, :text}, [identifier("a")]}])
  end

  test "select interval" do
    duration = Timex.Duration.parse!("P1Y2M3DT4H5M6S")
    assert_parse "select interval 'P1Y2M3DT4H5M6S' from bar",
      select(columns: [constant(:interval, ^duration)])
  end

  test "quoted identifier" do
    assert_parse "select `something that wouldn't normally work as a column name` from bar",
      select(columns: [identifier("something that wouldn't normally work as a column name")])
  end

  create_test =
    fn(description, data_source, statement, expected_error, line, column) ->
      test description do
        assert {:error, reason} = Parser.parse(unquote(data_source), unquote(statement))
        assert reason =~ unquote(expected_error)
        assert reason =~ "line #{unquote(line)}, column #{unquote(column)}\."
      end
    end

  Enum.each(
    [
      {"single quote is not allowed in the identifier",
        "select fo'o from baz", "Expected `from`", {1, 10}},
      {"identifier can't start with a number",
        "select 1foo from baz", "Expected `column definition`", {1, 8}},
      {"keyword is not identifier",
        "select select from baz", "Expected `column definition`", {1, 8}},
      {"from table is required",
        "select foo", "`from`", {1, 11}},
      {"at least one column must be specified",
        "select from baz", "Expected `column definition`", {1, 8}},
      {"columns must be separated with a comma",
        "select foo bar from baz", "Expected `from`", {1, 12}},
      {"query must start with a select or show",
        "foo select foo bar from baz", "Expected `select or show`", {1, 1}},
      {"show requires tables or columns",
        "show foobar", "Expected `tables or columns`", {1, 6}},
      {"show columns requires `from`",
        "show columns", "Expected `from`", {1, 13}},
      {"show columns requires a table",
        "show columns from", "Expected `table name`", {1, 18}},
      {"show columns works only with one table",
        "show columns from foo, bar", "Expected end of input", {1, 22}},
      {"!= is an illegal comparator in where clause",
        "select a from b where a != b", "Expected `comparator`", {1, 25}},
      {"=> is an illegal comparator in where clause",
        "select a from b where a => b", "Expected `comparison value`", {1, 26}},
      {"=< is an illegal comparator in where clause",
        "select a from b where a =< b", "Expected `comparison value`", {1, 26}},
      {"where clauses cannot be separated by or",
        "select a from b where a > 1 or b < 2", "Expected end of input", {1, 29}},
      {"not joining multiple where clauses is illegal",
        "select a from b where a > 1 b < 2", "Expected end of input", {1, 29}},
      {"on clause not allowed in a cross join",
        "select a from b cross join c on foo=bar", "Expected end of input", {1, 30}},
      {"count requires parens",
        "select count * from foo", "Expected `from`", {1, 14}},
      {"aggregation function requires parens",
          "select sum price from foo", "Expected `from`", {1, 12}},
      {"'by' has to follow 'order'",
        "select a from foo order a asc", "Expected `by`", {1, 25}},
      {"'by' has to follow 'group'",
        "select a from foo group a", "Expected `by`", {1, 25}},
      {"order by fields needs to be comma separated",
        "select a, b, c from foo order by a b", "Expected end of input", {1, 36}},
      {"invalid like",
        "select foo from bar where baz like", "Expected `string constant`", {1, 35}},
      {"invalid like type",
        "select foo from bar where baz like 10", "Expected `string constant`", {1, 36}},
      {"invalid in",
        "select foo from bar where baz in", "Expected `(`", {1, 33}},
      {"invalid is",
        "select foo from bar where baz is", "Expected `null`", {1, 33}},
      {"invalid comparison",
        "select foo from bar where baz =", "Expected `comparison value`", {1, 32}},
      {"missing where expression",
        "select foo from bar where", "Invalid where expression", {1, 26}},
      {"invalid where expression",
        "select foo from bar where foo bar", "Expected `comparator`", {1, 31}},
      {"no input allowed after the statement",
        "select foo from bar baz", "Expected end of input", {1, 21}},
      {"error after spaces",
        "   invalid_statement", "Expected `select or show`", {1, 4}},
      {"initial error after spaces and newlines",
        "  \n  \n invalid_statement", "Expected `select or show`", {3, 2}},
      {"assert at least one table",
        "select foo from", "Expected `table name`", {1, 16}},
      {"extended trim with two columns",
        "select trim(both a from b) from foo", "Expected `column definition`", {1, 8}},
      {"invalid interval",
        "select interval 'does not parse' from foo", "Expected `column definition`", {1, 8}},
      # parsed subqueries
      {"unclosed parens in a parsed subquery expression",
        "select foo from (select bar from baz", "Expected `)`", {1, 37}},
      {"empty parsed subquery expression",
        "select foo from ()", "Expected `select`", {1, 18}},
      {"missing alias in a parsed subquery expression",
        "select foo from (select bar from baz)", "Expected `subquery alias`", {1, 38}},
      {"missing alias after AS in an parsed subquery expression",
        "select foo from (select bar from baz) AS", "Expected `subquery alias`", {1, 41}},
      {"invalid subquery in a join",
        "select foo from bar cross join (select) alias", "Expected `column definition`", {1, 39}},
      # unparsed subqueries
      {"unclosed parens in an unparsed subquery expression", quote(do: @ds_proxy_data_source),
        "select foo from (select bar from baz", "Expected `)`", {1, 37}},
      {"empty unparsed subquery expression", quote(do: @ds_proxy_data_source),
        "select foo from ()", "Expected `subquery expression`", {1, 18}},
      {"missing alias in an unparsed subquery expression", quote(do: @ds_proxy_data_source),
        "select foo from (select bar from baz)", "Expected `subquery alias`", {1, 38}},
      {"missing alias after AS in an unparsed subquery expression", quote(do: @ds_proxy_data_source),
        "select foo from (select bar from baz) AS", "Expected `subquery alias`", {1, 41}},
    ],
    fn
      {description, statement, expected_error, {line, column}} ->
        create_test.(description, quote(do: @psql_data_source), statement, expected_error, line, column)
      {description, data_source, statement, expected_error, {line, column}} ->
        create_test.(description, data_source, statement, expected_error, line, column)
    end
  )
end
