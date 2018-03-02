defmodule Cloak.Sql.Parser.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Parser
  alias Cloak.Test.QueryHelpers


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
      assert unquote(expected_pattern) = Parser.parse!(unquote(query_string))
    end
  end

  defmacrop assert_equal_parse(query1, query2) do
    quote do
      result1 = unquote(query1) |> Parser.parse!() |> QueryHelpers.scrub_locations()
      result2 = unquote(query2) |> Parser.parse!() |> QueryHelpers.scrub_locations()

      assert result1 == result2
    end
  end

  defmacrop assert_parse_error(query_string, expected_pattern) do
    quote do
      assert {:error, unquote(expected_pattern)} = Parser.parse(unquote(query_string))
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
      {:constant, _, unquote(value), _}
    end
  end

  # Produces a pattern which matches an AST of a constant with type
  defmacrop constant(type, value) do
    quote do
      {:constant, unquote(type), unquote(value), _}
    end
  end

  # Produces a pattern which matches an AST of a constant with type and location
  defmacrop constant(type, value, location) do
    quote do
      {:constant, unquote(type), unquote(value), unquote(location)}
    end
  end

  # Produces a pattern which matches an AST of multiple constants.
  defmacrop constants(values) do
    Enum.map(values, &quote(do: constant(unquote(&1))))
  end

  defmacrop subquery(value, alias) do
    quote do
      {:subquery, %{ast: unquote(value), alias: unquote(alias)}}
    end
  end

  defmacrop identifier(name) do
    quote do
      {:identifier, _, unquoted(unquote(name)), _}
    end
  end

  defmacrop identifier(name, location) do
    quote do
      {:identifier, _, unquoted(unquote(name)), unquote(location)}
    end
  end

  defmacrop quoted_identifier(name) do
    quote do
      {:identifier, _, quoted(unquote(name)), _}
    end
  end

  defmacrop function(name, arguments) do
    quote do
      {:function, unquote(name), unquote(arguments), _}
    end
  end

  defmacrop function(name, arguments, location) do
    quote do
      {:function, unquote(name), unquote(arguments), unquote(location)}
    end
  end

  defmacrop unquoted(name) do
    quote do
      {:unquoted, unquote(name)}
    end
  end

  defmacrop quoted(name) do
    quote do
      {:quoted, unquote(name)}
    end
  end

  defmacrop cross_join(lhs, rhs) do
    quote do
      join(:cross_join, unquote(lhs), unquote(rhs), nil)
    end
  end

  defmacrop parameter(index), do:
    quote(do: {:parameter, index})

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
    assert_parse("select foo from baz", select(columns: [identifier("foo")], from: unquoted("baz")))
  end

  test "select distinct" do
    assert_parse("select distinct foo from bar", select(columns: [identifier("foo")], distinct?: true))
  end

  test "select all" do
    assert_parse("select all foo from bar", select(columns: [identifier("foo")], distinct?: false))
  end

  test "identifier location in source" do
    assert_parse("select foo\n, bar from baz", select(columns: [identifier("foo", {1, 8}), identifier("bar", {2, 3})]))
  end

  test "qualified identifier location in source" do
    assert_parse(~s[select baz.foo from baz], select(columns: [identifier("foo", {1, 8})]))
  end

  test "constant location in source" do
    assert_parse("select 1 from baz", select(columns: [constant(:integer, 1, {1, 8})]))
  end

  test "function location in source" do
    assert_parse("select foo(bar, baz) from baz", select(columns: [function("foo", _, {1, 8})]))
  end

  test "extract location in source" do
    assert_parse("select extract(month from foo) from bar", select(columns: [function("month", _, {1, 8})]))
  end

  test "trim location in source" do
    assert_parse("select trim(leading foo) from bar", select(columns: [function("ltrim", _, {1, 8})]))
  end

  test "substring location in source" do
    assert_parse("select substring(foo from 1) from bar", select(columns: [function("substring", _, {1, 8})]))
  end

  test "infix operator location in source" do
    assert_parse("select foo + bar from baz", select(columns: [function("+", _, {1, 12})]))
  end

  test "unary minus location in source" do
    assert_parse("select -foo(bar) from baz", select(columns: [function("-", _, {1, 8})]))
  end

  test "cast location in source" do
    assert_parse("select cast(foo as text) from bar", select(columns: [function({:cast, :text}, _, {1, 8})]))
  end

  test "cast with :: location in source" do
    assert_parse("select foo :: text from bar", select(columns: [function({:cast, :text}, _, {1, 12})]))
  end

  test "bucket location in source" do
    assert_parse("select bucket(foo by 10) from bar", select(columns: [function({:bucket, _}, _, {1, 8})]))
  end

  test "fully qualified table name" do
    assert_parse("select foo from bar.baz",
      select(columns: [identifier("foo")], from: unquoted("bar.baz")))
  end

  test "quoted fully qualified table name" do
    assert_parse("select foo from \"bar\".baz",
      select(columns: [identifier("foo")], from: quoted("bar.baz")))
  end

  test "query with a terminating semicolon" do
    assert_parse("select foo from baz;",
      select(columns: [identifier("foo")], from: unquoted("baz")))
  end

  test "multiple fields" do
    assert_parse("select foo, bar from baz",
      select(columns: [identifier("foo"), identifier("bar")], from: unquoted("baz")))
  end

  test "all fields" do
    assert_parse("select * from baz", select(columns: [:*], from: unquoted("baz")))
  end

  test "whitespaces are ignored" do
    assert_parse("select  foo\n from \n \n baz  \n ; \n  ",
      select(columns: [identifier("foo")], from: unquoted("baz")))
  end

  test "unicode whitespaces are ignored" do
    assert_parse("select\u202Ffoo\u00A0from\u202Fbaz",
      select(columns: [identifier("foo")], from: unquoted("baz")))
  end

  test "all allowed identifier characters" do
    assert_parse("select foO1_ from Ba_z2",
      select(columns: [identifier("foO1_")], from: unquoted("Ba_z2")))
  end

  test "case insensivity of commands" do
    assert_parse("SELECT foo FROM baz", select(columns: [identifier("foo")], from: unquoted("baz")))
  end

  test "show tables" do
    assert_parse("show tables", show(:tables))
  end

  test "show columns" do
    assert_parse("show columns from foo", show(:columns, from: unquoted("foo")))
  end

  test "where clause with equality" do
    assert_parse(
      "select foo from bar where a = 10",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :=, constant(10)})
    )
  end

  test "where clause with <" do
    assert_parse(
      "select foo from bar where a < 10",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :<, constant(10)})
    )
  end

  test "where clause with >" do
    assert_parse(
      "select foo from bar where a > 10",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :>, constant(10)})
    )
  end

  test "where clause with >=" do
    assert_parse(
      "select foo from bar where a >= 10",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :>=, constant(10)})
    )
  end

  test "where clause with <=" do
    assert_parse(
      "select foo from bar where a <= 10",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :<=, constant(10)})
    )
  end

  test "where clause with <>" do
    assert_parse(
      "select foo from bar where a <> 10",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :<>, constant(10)})
    )
  end

  test "where clause can have float values" do
    assert_parse(
      "select foo from bar where a = 10.0",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :=, constant(10.0)})
    )
  end

  test "where clause can have string values" do
    assert_parse(
      "select foo from bar where name = 'tom'",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("name"), :=, constant("tom")})
    )
  end

  test "where clause comparing two columns" do
    assert_parse(
      "select foo from bar where a = b",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("a"), :=, identifier("b")})
    )
  end

  test "where clause can have string values of any case" do
    assert_parse(
      "select foo from bar where name = 'tOm'",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("name"), :=, constant("tOm")})
    )
  end

  test "where clause can have multi-word string values" do
    assert_parse(
      "select foo from bar where name = 'avishai cohen'",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:comparison, identifier("name"), :=, constant("avishai cohen")})
    )
  end

  test "where clause with mutliple comparisons" do
    assert_parse(
      "select foo from bar where a <> 10 and b = 'bar'",
      select(
        columns: [identifier("foo")], from: unquoted("bar"),
        where: {:and,
          {:comparison, identifier("a"), :<>, constant(10)},
          {:comparison, identifier("b"), :=, constant("bar")}
        }
      )
    )
  end

  test "where clause with BETWEEN" do
    assert_parse(
      "select foo from bar where a between 10 and 20",
      select(
        columns: [identifier("foo")], from: unquoted("bar"),
        where: {:and,
          {:comparison, identifier("a"), :>=, constant(10)},
          {:comparison, identifier("a"), :<, constant(20)}
        }
      )
    )
  end

  test "where clause with LIKE" do
    assert_parse(
      "select foo from bar where a LIKE '_ob d%'",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:like, identifier("a"), {:like_pattern, constant("_ob d%"), constant(nil)}})
    )
  end

  test "where clause with NOT LIKE" do
    assert_parse(
      "select foo from bar where a NOT LIKE '%pattern%'",
      select(where: {:not, {:like, identifier("a"), {:like_pattern, constant("%pattern%"), constant(nil)}}})
    )
  end

  test "where clause with ILIKE" do
    assert_parse(
      "select foo from bar where a ILIKE '_ob d%'",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:ilike, identifier("a"), {:like_pattern, constant("_ob d%"), constant(nil)}})
    )
  end

  test "where clause with NOT ILIKE" do
    assert_parse(
      "select foo from bar where a NOT ILIKE '%pattern%'",
      select(where: {:not, {:ilike, identifier("a"), {:like_pattern, constant("%pattern%"), constant(nil)}}})
    )
  end

  for word <- ~w(like ilike) do
    test "#{word} with an escape character" do
      assert_parse(
        "select foo from bar where baz #{unquote(word)} '\\%pattern%' escape '\\'",
        select(where: {_, identifier("baz"), {:like_pattern, constant("\\%pattern%"), constant("\\")}})
      )
    end

    test "not #{word} with an escape character" do
      assert_parse(
        "select foo from bar where baz not #{unquote(word)} '\\%pattern%' escape '\\'",
        select(where: {:not, {_, identifier("baz"), {:like_pattern, constant("\\%pattern%"), constant("\\")}}})
      )
    end
  end

  test "where clause with IN" do
    assert_parse(
      "select foo from bar where a IN (1, 2, 3)",
      select(columns: [identifier("foo")], from: unquoted("bar"),
        where: {:in, identifier("a"), constants([1, 2, 3])})
    )
  end

  test "where clause with IS and IS NOT" do
    assert_parse(
      "select foo from bar where a is null and b is not null",
      select(where: {:and, {:is, identifier("a"), :null}, {:not, {:is, identifier("b"), :null}}})
    )
  end

  test "where clause with all types of clauses" do
    assert_parse(
      "select foo from bar where a = 2 and b in (1,2,3) and c like '_o' and d is not null",
      select(
        columns: [identifier("foo")], from: unquoted("bar"),
        where:
          {:and,
            {:and,
              {:and,
                {:comparison, identifier("a"), :=, constant(2)},
                {:in, identifier("b"), constants([1, 2, 3])}
              },
              {:like, identifier("c"), {:like_pattern, constant("_o"), constant(nil)}}
            },
            {:not, {:is, identifier("d"), :null}}
          }
      )
    )
  end

  test "where clause with parens" do
    assert_equal_parse(
      "select foo from bar where (a = 2 and b = 3)",
      "select foo from bar where a = 2 and b = 3"
    )
  end

  test "where sub-clause with parens" do
    assert_equal_parse(
      "select foo from bar where (((a = 1) and b = 2) and c = 3)",
      "select foo from bar where a = 1 and b = 2 and c = 3"
    )
  end

  test "boolean values are allowed in comparisons" do
    assert_parse(
      "select foo from bar where a = true and b in (true, false)",
      select(
        columns: [identifier("foo")], from: unquoted("bar"),
        where: {:and, {:comparison, identifier("a"), :=, constant(true)},
          {:in, identifier("b"), constants([true, false])}}
      )
    )
  end

  test "having clause with parens" do
    assert_equal_parse(
      "select foo from bar having (a = 1 and b = 3) and (c = 4)",
      "select foo from bar having a = 1 and b = 3 and c = 4"
    )
  end

  test "it's valid to have a identifier contain a keyword" do
    assert_parse(
      "SELECT INvalid, selectiscious FROM whereables",
      select(columns: [identifier("INvalid"),
        identifier("selectiscious")], from: unquoted("whereables"))
    )
  end

  test "date extraction" do
    assert_parse(
      "SELECT extract(year FROM column) FROM table",
      select(columns: [function("year", [identifier("column")])])
    )
  end

  test "count(*)" do
    assert_parse("select count(*) from foo", select(columns: [function("count", [:*])], from: unquoted("foo")))
  end

  test "aggregation functions" do
    assert_parse(
      "select sum(price), min(value) from foo",
      select(columns: [function("sum", [identifier("price")]),
        function("min", [identifier("value")])], from: unquoted("foo"))
    )
  end

  test "group by multiple columns" do
    assert_parse(
      "select x from b group by x, y, z",
      select(columns: [identifier("x")], from: unquoted("b"),
        group_by: [identifier("x"), identifier("y"), identifier("z")])
    )
  end

  test "group by just one column" do
    assert_parse("select a from b group by a", select(group_by: [identifier("a")]))
  end

  test "order by clause" do
    assert_parse(
      "select a, b, c from foo order by a desc, b asc nulls first, c nulls last, d asc",
      select(columns: [
          identifier("a"), identifier("b"), identifier("c")
        ], from: unquoted("foo"), order_by: [
          {identifier("a"), :desc, :nulls_natural}, {identifier("b"), :asc, :nulls_first},
          {identifier("c"), :asc, :nulls_last}, {identifier("d"), :asc, :nulls_natural}
        ])
    )
  end

  test "parsed subquery sql" do
    assert_parse(
      "select foo from (select foo from bar) alias",
      select(columns: [identifier("foo")], from: subquery(subquery, "alias"))
    )
    assert select(columns: [identifier("foo")], from: unquoted("bar")) = subquery
  end

  test "parsed subquery with a quoted alias" do
    assert_parse(
      "select foo from (select foo from bar) \"ali as\"",
      select(from: subquery(_, "ali as"))
    )
  end

  test "parsed nested subquery sql" do
    assert_parse(
      "select foo from (select foo from (select foo from bar) inner_alias) outer_alias",
      select(columns: [identifier("foo")], from: subquery(subquery, "outer_alias"))
    )

    assert select(
      columns: [identifier("foo")],
      from: subquery(inner_subquery, "inner_alias")
    ) = subquery

    assert select(columns: [identifier("foo")], from: unquoted("bar")) = inner_subquery
  end

  test "join of parsed subqueries" do
    assert_parse(
      "select foo from (select foo from bar) sq1, (select foo from baz) sq2",
      select(
        columns: [identifier("foo")],
        from: cross_join(subquery(sq1, "sq1"), subquery(sq2, "sq2"))
      )
    )
    assert select(columns: [identifier("foo")], from: unquoted("bar")) = sq1
    assert select(columns: [identifier("foo")], from: unquoted("baz")) = sq2
  end

  test "joining table with a parsed subquery" do
    assert_parse(
      "select foo from bar inner join (select foo from baz) sq on bar.id = sq.id",
      select(
        columns: [identifier("foo")],
        from: inner_join(unquoted("bar"), subquery(sq, "sq"), _comparison)
      )
    )
    assert select(columns: [identifier("foo")], from: unquoted("baz")) = sq
  end

  test "join condition with parens" do
    assert_equal_parse(
      "select foo from bar inner join baz on (a = 1 and (b = 2))",
      "select foo from bar inner join baz on a = 1 and b = 2"
    )
  end

  test "count(distinct column)" do
    assert_parse(
      "select count(distinct foo) from bar",
      select(columns: [function("count", [{:distinct, identifier("foo")}])])
    )
  end

  test "count(all column)" do
    assert_parse(
      "select count(all foo) from bar",
      select(columns: [function("count", [identifier("foo")])])
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
          identifier("column"),
          function("count", [identifier("column")]),
          function("count", [{:distinct, identifier("column")}])
        ],
        where: {:comparison, identifier("column"), :=, _},
        group_by: [identifier("column")],
        order_by: [
          {identifier("column"), :desc, :nulls_natural},
          {function("count", [{:distinct, identifier("column")}]), :asc, :nulls_natural}
        ]
      )
    )
  end

  test "allow selection of multiple tables" do
    assert_parse("select a from foo, bar, baz",
      select(columns: [identifier("a")],
        from: cross_join(unquoted("foo"), cross_join(unquoted("bar"), unquoted("baz")))))
  end

  test "allow CROSS JOINs" do
    assert_parse("select a from foo CROSS JOIN bar",
      select(columns: [identifier("a")], from: cross_join(unquoted("foo"), unquoted("bar"))))
  end

  test "allow multiple CROSS JOINs" do
    assert_parse("select a from t1 CROSS JOIN t2 CROSS JOIN t3 CROSS JOIN t4",
      select(columns: [identifier("a")], from:
        cross_join(cross_join(cross_join(unquoted("t1"), unquoted("t2")), unquoted("t3")), unquoted("t4"))
      ))
  end

  test "allow INNER JOINs" do
    assert_parse("select a from foo JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: inner_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
    assert_parse("select a from foo INNER JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: inner_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
  end

  test "allow multiple INNER JOINs" do
    assert_parse("select a from foo JOIN bar ON a = b INNER JOIN baz ON b = c",
      select(columns: [identifier("a")],
        from: inner_join(
            inner_join(unquoted("foo"), unquoted("bar"),
            {:comparison, identifier("a"), :=, identifier("b")}
          ), unquoted("baz"),
          {:comparison, identifier("b"), :=, identifier("c")}
        )))
  end

  test "allow LEFT JOINs" do
    assert_parse("select a from foo LEFT JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: left_outer_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
    assert_parse("select a from foo LEFT OUTER JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: left_outer_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
  end

  test "allow RIGHT JOINs" do
    assert_parse("select a from foo RIGHT JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: right_outer_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
    assert_parse("select a from foo RIGHT OUTER JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: right_outer_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
  end

  test "allow FULL OUTER JOINs" do
    assert_parse("select a from foo FULL JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: full_outer_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
    assert_parse("select a from foo FULL OUTER JOIN bar ON a = b",
      select(columns: [identifier("a")],
        from: full_outer_join(unquoted("foo"), unquoted("bar"),
          {:comparison, identifier("a"), :=, identifier("b")}
        )))
  end

  test "allow combining JOIN types" do
    assert_parse("select a from t1, t2 JOIN t3 ON a = b CROSS JOIN t4, t5 CROSS JOIN t6",
      select(columns: [identifier("a")],
        from: cross_join(unquoted("t1"),
                cross_join(
                  cross_join(
                    inner_join(unquoted("t2"), unquoted("t3"),
                      {:comparison, identifier("a"), :=, identifier("b")}
                    ),
                    unquoted("t4")
                  ),
                  cross_join(unquoted("t5"), unquoted("t6"))))))
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
      select(columns: [{identifier("a"), :as, "x"}]))
  end

  test "function in group by" do
    assert_parse("select * from foo group by bar(x)",
      select(group_by: [function("bar", [identifier("x")])])
    )
  end

  test "select a constant" do
    assert_parse("select 10 from foo", select(columns: [constant(:integer, 10)]))
  end

  test "multi-argument function" do
    assert_parse("select foo(x, y, z) from bar", select(columns:
      [function("foo", [identifier("x"), identifier("y"), identifier("z")])]
    ))
  end

  test "extended btrim" do
    assert_parse "select trim(both from foo) from bar",
      select(columns: [function("btrim", [identifier("foo")])])
  end

  test "extended ltrim" do
    assert_parse "select trim(leading from foo) from bar",
      select(columns: [function("ltrim", [identifier("foo")])])
  end

  test "extended rtrim" do
    assert_parse "select trim(trailing from foo) from bar",
      select(columns: [function("rtrim", [identifier("foo")])])
  end

  test "extended with character set" do
    assert_parse "select trim(both 'xyz' from foo) from bar",
      select(columns: [function("btrim", [identifier("foo"), constant(:string, "xyz")])])
  end

  test "substring from" do
    assert_parse "select substring(foo from 3) from bar",
      select(columns: [function("substring", [identifier("foo"), constant(:integer, 3)])])
  end

  test "substr" do
    assert_parse "select substr(foo from 3) from bar",
      select(columns: [function("substring", [identifier("foo"), constant(:integer, 3)])])
  end

  test "substring from ... for ..." do
    assert_parse "select substring(foo from 3 for 10) from bar",
      select(columns: [function("substring", [
        identifier("foo"), constant(:integer, 3), constant(:integer, 10)])])
  end

  test "substring for" do
    assert_parse "select substring(foo for 3) from bar",
      select(columns: [function("substring", [identifier("foo"), constant(:integer, 1), constant(:integer, 3)])])
  end

  test "|| compiled as concat" do
    assert_parse "select a || b || c from bar",
      select(columns: [function("concat", [
        function("concat", [identifier("a"), identifier("b")]),
        identifier("c")])])
  end

  test "|| of complex expressions" do
    assert_parse "select lower(a) || upper(b) from bar",
      select(columns: [function("concat", [
        function("lower", [identifier("a")]),
        function("upper", [identifier("b")])])])
  end

  test "+ and -" do
    assert_parse "select a + b - c + d from bar",
      select(columns: [function("+", [
        function("-", [
          function("+", [identifier("a"), identifier("b")]),
          identifier("c")]),
        identifier("d")])])
  end

  test "*, /, and %" do
    assert_parse "select a * b / c % d from bar",
      select(columns: [function("%", [
        function("/", [
          function("*", [identifier("a"), identifier("b")]),
          identifier("c")]),
        identifier("d")])])
  end

  test "^" do
    assert_parse "select a ^ b ^ c from bar",
      select(columns: [function("^", [
        function("^", [identifier("a"), identifier("b")]),
        identifier("c")])])
  end

  test "()" do
    assert_parse "select a ^ ((b + c) * (d - e)) from foo",
      select(columns: [function("^", [
        identifier("a"),
        function("*", [
          function("+", [identifier("b"), identifier("c")]),
          function("-", [identifier("d"), identifier("e")])])])])
  end

  test "* and / have higher precedence than + and -" do
    assert_parse "select a * b + c / d - e from bar",
      select(columns: [function("-", [
        function("+", [
          function("*", [identifier("a"), identifier("b")]),
          function("/", [identifier("c"), identifier("d")])]),
        identifier("e")])])
  end

  test "^ has a higher precedence than *" do
    assert_parse "select a ^ b * c from bar",
      select(columns: [function("*", [
        function("^", [identifier("a"), identifier("b")]),
        identifier("c")])])
  end

  test "unary minus" do
    assert_parse "select -a from bar",
      select(columns: [function("-", [constant(:integer, 0), identifier("a")])])
  end

  test "unary plus" do
    assert_parse "select +a from bar", select(columns: [identifier("a")])
  end

  test "unary minus has higher precedence than ^" do
    assert_parse "select - a ^ - b from bar",
      select(columns: [function("^", [
        function("-", [constant(:integer, 0), identifier("a")]),
        function("-", [constant(:integer, 0), identifier("b")])])])
  end

  test "cast" do
    assert_parse "select cast(a, integer) from bar",
      select(columns: [function({:cast, :integer}, [identifier("a")])])
  end

  test "cast to interval", do:
    assert_parse "select cast(a, interval) from bar", select(columns: [function({:cast, :interval}, _)])

  test "extended cast" do
    assert_parse "select cast(a as text) from bar",
      select(columns: [function({:cast, :text}, [identifier("a")])])
  end

  test "cast to datetime", do:
    assert_parse "select cast(a as datetime) from bar", select(columns: [function({:cast, :datetime}, _)])

  test "cast to timestamp" do
    result1 = Parser.parse!("select cast(a as datetime) from bar")
    result2 = Parser.parse!("select cast(a as timestamp) from bar")

    assert result1 == result2
  end

  test "cast to float", do:
    assert_parse "select cast(a as float) from bar", select(columns: [function({:cast, :real}, _)])

  test "cast to double precision", do:
    assert_parse "select cast(a as double precision) from bar", select(columns: [function({:cast, :real}, _)])

  describe "::" do
    test "cast with ::", do:
      assert_parse "select a::integer from bar",
        select(columns: [function({:cast, :integer}, [identifier("a")])])

    test "multiple casts with ::", do:
      assert_parse "select a::integer::text from bar",
        select(columns: [function({:cast, :text}, [function({:cast, :integer}, [identifier("a")])])])

    test "a non-datatype on RHS of ::", do:
      assert {:error, _} = Parser.parse("select a::b from bar")
  end

  for word <- ~w(date time datetime timestamp) do
    test "#{word} as a column name" do
      assert_parse "select #{unquote(word)} from bar",
        select(columns: [identifier(unquote(word))])
    end
  end

  test "select interval" do
    duration = Timex.Duration.parse!("P1Y2M3DT4H5M6S")
    assert_parse "select interval 'P1Y2M3DT4H5M6S' from bar",
      select(columns: [constant(:interval, ^duration)])
  end

  test "quoted identifier" do
    assert_parse "select \"something that wouldn't normally work as a column name\" from bar",
      select(columns: [quoted_identifier("something that wouldn't normally work as a column name")])
  end

  test "string literals containing escaped single-quotes" do
    assert_parse "select 'O''Brian' from names", select(columns: [constant(:string, "O'Brian")])
    assert_parse "select 'O'\n'Brian' from names", select(columns: [constant(:string, "OBrian")])
    assert_parse ~S(select 'O\Brian' from names), select(columns: [constant(:string, ~S(O\Brian))])
  end

  for type <- ~w(lower upper middle)a do
    test "bucket(*, *, #{type})" do
      assert_parse "select bucket(foo by 10 align #{unquote(type)}) from bar", select(columns: [
        function({:bucket, unquote(type)}, [identifier("foo"), constant(:integer, 10)])
      ])
    end
  end

  for {type, synonym} <- [{:lower, :bottom}, {:upper, :top}] do
    test "bucket(*, *, #{synonym}) is bucket(*, *, #{type})" do
      assert_equal_parse "select bucket(foo by 10 align #{unquote(type)}) from bar",
        "select bucket(foo by 10 align #{unquote(synonym)}) from bar"
    end
  end

  test "bucket aligns lower by default" do
    assert_equal_parse "select bucket(foo by 10) from bar", "select bucket(foo by 10 align lower) from bar"
  end

  test "align type case insensivity" do
    assert_parse "select bucket(foo by 10 align MIDDlE) from bar", select(columns: [
      function({:bucket, :middle}, [identifier("foo"), constant(:integer, 10)])
    ])
  end

  test "query parameters", do:
    assert_parse(
      "select $1, $2 + 1 FROM foo WHERE $3 = $4",
      select(
        columns: [parameter(1), function("+", [parameter(2), constant(1)])],
        where: {:comparison, parameter(3), :=, parameter(4)}
      )
    )

  test "parsing of integer with signs" do
    assert_parse(
      "select 1+2, -3, +4, -5--6, -7++8, 9 + 10 FROM foo",
      select(
        columns: [
          function("+", [constant(1), constant(2)]),
          function("-", [constant(0), constant(3)]),
          constant(4),
          function("-", [
            function("-", [constant(0), constant(5)]),
            function("-", [constant(0), constant(6)]),
          ]),
          function("+", [function("-", [constant(0), constant(7)]), constant(8)]),
          function("+", [constant(9), constant(10)])
        ]
      )
    )
  end

  test "parsing of floats with signs" do
    assert_parse(
      "select 1.1+2.1, -3.1, +4.1, -5.1--6.1, -7.1++8.1, 9.1 + 10.1 FROM foo",
      select(
        columns: [
          function("+", [constant(1.1), constant(2.1)]),
          function("-", [constant(0), constant(3.1)]),
          constant(4.1),
          function("-", [
            function("-", [constant(0), constant(5.1)]),
            function("-", [constant(0), constant(6.1)])
          ]),
          function("+", [function("-", [constant(0), constant(7.1)]), constant(8.1)]),
          function("+", [constant(9.1), constant(10.1)])
        ]
      )
    )
  end

  test "parsing of `or` conditions" do
    assert_parse(
      "select count(*) from x where a > 0 or a <> 2",
      select(where:
        {:or,
          {:comparison, identifier("a"), :>, constant(0)},
          {:comparison, identifier("a"), :<>, constant(2)}
        }
      )
    )
  end

  test "parsing of `and` / `or` in WHERE conditions (1)" do
    assert_parse(
      "select count(*) from x where a > 0 or a <> 2 and b = 3",
      select(where:
        {:or,
          {:comparison, identifier("a"), :>, constant(0)},
          {:and,
            {:comparison, identifier("a"), :<>, constant(2)},
            {:comparison, identifier("b"), :=, constant(3)}
          }
        }
      )
    )
  end

  test "parsing of `and` / `or` in WHERE conditions (2)" do
    assert_parse(
      "select count(*) from x where a > 0 and a <> 2 or b = 3",
      select(where:
        {:or,
          {:and,
            {:comparison, identifier("a"), :>, constant(0)},
            {:comparison, identifier("a"), :<>, constant(2)}
          },
          {:comparison, identifier("b"), :=, constant(3)}
        }
      )
    )
  end

  test "parsing of `and` / `or` in HAVING conditions" do
    assert_parse(
      "select count(*) from x having a > 0 or a <> 2 and b = 3",
      select(having:
        {:or,
          {:comparison, identifier("a"), :>, constant(0)},
          {:and,
            {:comparison, identifier("a"), :<>, constant(2)},
            {:comparison, identifier("b"), :=, constant(3)}
          }
        }
      )
    )
  end

  test "parsing of `or` in ON conditions" do
    assert_parse(
      "select count(*) from x inner join y on a = b or a = c",
      select(from: inner_join(
        unquoted("x"),
        unquoted("y"),
        {:or,
          {:comparison, identifier("a"), :=, identifier("b")},
          {:comparison, identifier("a"), :=, identifier("c")},
        })
      )
    )
  end

  test "parsing of `and` / `or` / parens conditions" do
    assert_parse(
      "select count(*) from x where (a > 0 or a <> 2) and b = 3",
      select(where:
        {:and,
          {:or,
            {:comparison, identifier("a"), :>, constant(0)},
            {:comparison, identifier("a"), :<>, constant(2)}
          },
          {:comparison, identifier("b"), :=, constant(3)}
        }
      )
    )
  end

  test "parsing of `and` / `or` / extra parens conditions (1)" do
    assert_parse(
      "select count(*) from x having ((a > 0) or (a <> 2)) and (b = 3)",
      select(having:
        {:and,
          {:or,
            {:comparison, identifier("a"), :>, constant(0)},
            {:comparison, identifier("a"), :<>, constant(2)}
          },
          {:comparison, identifier("b"), :=, constant(3)}
        }
      )
    )
  end

  test "parsing of `and` / `or` / extra parens conditions (2)" do
    assert_parse(
      "select count(*) from x having (a > 0 or (a <> 2)) and b = 3",
      select(having:
        {:and,
          {:or,
            {:comparison, identifier("a"), :>, constant(0)},
            {:comparison, identifier("a"), :<>, constant(2)}
          },
          {:comparison, identifier("b"), :=, constant(3)}
        }
      )
    )
  end

  test "parsing comments" do
    assert_parse("""
      -- Full line comment
      SELECT * -- Partial line comment, foo as bar
      FROM baz
      -- Comment at the end...
    """, select(columns: [:*], from: unquoted("baz")))
  end

  test "select with a table alias", do:
    assert_parse("select foo from baz b", select(columns: [identifier("foo")], from: {unquoted("baz"), :as, "b"}))

  test "select with a table alias 2", do:
    assert_parse("select foo from baz as b", select(columns: [identifier("foo")], from: {unquoted("baz"), :as, "b"}))

  test "select all from a table", do:
    assert_parse("select foo.* from foo", select(columns: [{:*, "foo"}]))

  test "select all from a quoted table", do:
    assert_parse("select \"foo bar\".* from foo", select(columns: [{:*, "foo bar"}]))

  test "select all from a table in a comma-separated list", do:
    assert_parse("select x, foo.*, y, bar.* from foo",
      select(columns: [identifier("x"), {:*, "foo"}, identifier("y"), {:*, "bar"}]))

  test "sample from table", do:
    assert_parse("select x from foo sample_users 10%", select(sample_rate: 10))

  describe "unary NOT" do
    test "with a simple condition", do:
      assert_parse("select * from foo where NOT x = 1", select(where: {
        :not, {:comparison, identifier("x"), :=, constant(1)}}))

    test "with a complex condition", do:
      assert_parse("select * from foo where NOT (x = 1 OR y = 2)", select(where: {:not, {:or,
        {:comparison, identifier("x"), :=, constant(1)},
        {:comparison, identifier("y"), :=, constant(2)}
      }}))

    test "in HAVING", do:
      assert_parse("select count(*) from foo having NOT (x = 1 OR y = 2)", select(having: {:not, {:or,
        {:comparison, identifier("x"), :=, constant(1)},
        {:comparison, identifier("y"), :=, constant(2)}
      }}))

    test "in ON", do:
      assert_parse("select count(*) from foo join bar ON NOT a = b", select(from: {:join, %{conditions: {:not,
        {:comparison, identifier("a"), :=, identifier("b")}}}}))

    test "many NOTs", do:
      assert_parse("select count(*) from foo having NOT NOT NOT x = 1", select(having: {:not, {:not, {:not,
        {:comparison, identifier("x"), :=, constant(1)}}}}))
  end

  create_test =
    fn(description, statement, expected_error, line, column) ->
      test description do
        assert {:error, reason} = Parser.parse(unquote(statement))
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
        "select a from b where a != b", "Expected `equality comparator`", {1, 25}},
      {"=> is an illegal comparator in where clause",
        "select a from b where a => b", "Expected `comparison value`", {1, 26}},
      {"=< is an illegal comparator in where clause",
        "select a from b where a =< b", "Expected `comparison value`", {1, 26}},
      {"not joining multiple where clauses is illegal",
        "select a from b where a > 1 b < 2", "Expected end of input", {1, 29}},
      {"on clause not allowed in a cross join",
        "select a from b cross join c on foo=bar", "Expected end of input", {1, 30}},
      {"count requires parens",
        "select count * from foo", "Expected `column definition`", {1, 16}},
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
        "select foo from bar where", "Expected `equality comparator`", {1, 26}},
      {"invalid where expression",
        "select foo from bar where foo bar", "Expected `equality comparator`", {1, 31}},
      {"no input allowed after the statement",
        "select foo from bar baz qux", "Expected end of input", {1, 25}},
      {"error after spaces",
        "   invalid_statement", "Expected `select or show`", {1, 4}},
      {"initial error after spaces and newlines",
        "  \n  \n invalid_statement", "Expected `select or show`", {3, 2}},
      {"assert at least one table",
        "select foo from", "Expected `table name`", {1, 16}},
      {"extended trim with two columns",
        "select trim(both a from b) from foo", "Expected `)`", {1, 20}},
      {"invalid interval",
        "select interval 'does not parse' from foo", "Expected `column definition`", {1, 8}},
      {"inequality between two columns",
        "select count(*) from foo where bar < baz", "Expected `constant`", {1, 38}},
      {"table can't be parameterized",
        "select x from $1", "Expected `table name`", {1, 15}},
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
      {"invalid column other than the first one",
        "select foo, & from foo", "Expected `column definition`", {1, 13}},
      {"error inside an item in the select list",
        "select foo, cast(3 as) from foo", "Expected `type name`", {1, 22}},
      {"wrong cast",
        "select cast(foo as bar) from baz", "Expected `type name`", {1, 20}},
      {"bucket size is not constant",
        "select bucket(foo by bar) from baz", "Expected `numeric constant`", {1, 22}},
      {"bad bucket align part",
        "select bucket(foo by 10 by) from baz", "Expected `)`", {1, 25}},
      {"error on incomplete qualified select column",
        "select foo. from bar", "Expected `from`", {1, 11}},
      {"invalid quoted column name",
        "select \"x\".\"y\".\"z\" from baz", "Expected `from`", {1, 15}},
      {"invalid mixed quote column name",
        "select \"x\".y.\"z\" from baz", "Expected `from`", {1, 13}},
      {"invalid nulls directive",
        "select foo from bar order by baz nulls whatever", "Expected `one of first, last`", {1, 40}},
      {"nulls directive with a quoted identifier",
        "select foo from bar order by baz nulls \"first\"", "Expected `one of first, last`", {1, 40}},
      {"incomplete substring in where",
        "select * from foo where substring(lower(bar, 1, 1) = 3", "Expected `substring arguments`", {1, 52}},
      {"inequality with non-constant RHS",
        "select * from foo where bar < baz + 1 and x = 1", "Expected `constant`", {1, 31}},
      {"condition with non-constant RHS at end of boolean expression",
        "select * from foo where x = 1 and bar < baz + 1", "Expected `constant`", {1, 41}},
      {"invalid extract part",
        "select extract(invalid from date) from table", "Expected `date part`", {1, 16}},
    ],
    fn
      {description, statement, expected_error, {line, column}} ->
        create_test.(description, statement, expected_error, line, column)
    end
  )

  describe "parse!" do
    test "returns parsed query if OK", do:
      assert select([]) = Parser.parse!("select foo from bar")

    test "raises ParseError with location if error" do
      error = try do
        Parser.parse!("select select")
        nil
      rescue
        e in Cloak.Sql.ParseError -> e
      end

      assert %Cloak.Sql.ParseError{message: "Expected `column definition`.", source_location: {1, 8}} = error
    end
  end
end
