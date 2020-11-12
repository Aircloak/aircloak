defmodule Cloak.Sql.Parser.Internal do
  @moduledoc "Parser for SQL queries."
  import Combine.Parsers.Base
  import Cloak.Sql.Parser.Parsers

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses an SQL query in text form."
  @spec parse(String.t()) :: {:ok, Cloak.Sql.Parser.parsed_query()} | {:error, any}
  def parse(statement) do
    with {:ok, tokens} <- Cloak.Sql.Lexer.tokenize(statement) do
      case Combine.parse(tokens, parser()) do
        {:error, _} = error -> error
        [ast] -> {:ok, ast}
      end
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parser() do
    init_token_parser()
    |> statement()
    |> statement_termination()
    |> end_of_input()
  end

  defp statement(parser) do
    parser
    |> switch([
      {keyword(:explain), parenthesised_select_statement()},
      {keyword(:show), show_statement()},
      {:else, parenthesised_select_statement("select, explain, or show")}
    ])
    |> map(fn
      {[command], [statement_data]} -> statement_data |> Keyword.put(:command, command) |> statement_map()
      statement_data -> statement_map(statement_data)
    end)
  end

  defp statement_map(statement_data) do
    statement_data |> Enum.reject(fn value -> value == nil end) |> Enum.into(%{})
  end

  defp statement_termination(parser) do
    parser
    |> skip(keyword(:";"))
  end

  defp show_statement() do
    switch([
      {keyword(:tables), noop()},
      {keyword(:columns), pair_both(keyword(:from), table_name())},
      {:else, error_message(fail(""), "Expected `tables or columns`")}
    ])
    |> map(fn {[show], data} -> [{:show, show} | data] end)
  end

  defp select_statement() do
    sequence([
      optional_distinct(),
      select_columns(),
      from(),
      optional_where(),
      optional_group_by(),
      optional_having(),
      optional_order_by(),
      optional_limit(),
      optional_offset()
    ])
    |> map(&[{:command, :select} | &1])
  end

  defp parenthesised_select_statement(prefix_label \\ "select") do
    either_deepest_error(
      between(keyword(:"("), lazy(fn -> parenthesised_select_statement() end), subquery_close_paren()),
      pair_both(keyword_with_position(:select) |> label(prefix_label), pair_both(select_statement(), position()))
      |> map(fn {{start_pos, :select}, {statement, end_pos}} -> [{:source_range, {start_pos, end_pos}} | statement] end)
    )
    |> sep_by1_eager(union())
    |> map(fn [first | rest] ->
      rest
      |> Enum.chunk_every(2)
      |> Enum.reduce(first, fn [{:union, type}, right_statement], left_statement ->
        [
          command: :union,
          distinct?: type != :all,
          from:
            {:union, {:subquery, %{ast: statement_map(left_statement), alias: nil}},
             {:subquery, %{ast: statement_map(right_statement), alias: nil}}}
        ]
      end)
    end)
  end

  defp union() do
    pair_both(keyword(:union), option(keyword_of([:all, :distinct])))
  end

  defp select_columns() do
    map(comma_delimited(select_column()), &{:columns, &1})
  end

  defp column() do
    lazy(fn -> disjunction_expression(condition_expression()) end)
  end

  defp condition_expression() do
    pair_both(
      additive_expression(),
      switch([
        {option(keyword_with_position(:not)) |> keyword_of_with_position([:like, :ilike]),
         sequence([constant(:string), like_escape()])},
        {option(keyword_with_position(:not)) |> keyword_with_position(:in), in_values()},
        {next_position() |> keyword(:is) |> option(keyword_with_position(:not)), keyword(:null)},
        {next_position() |> option(keyword(:not)) |> keyword(:between), comparison_range()},
        {next_position() |> keyword_of([:<, :<=, :>, :>=, :=, :<>]), not_expression(comparison_value())},
        {:else, return(nil)}
      ])
    )
    |> map(fn
      {lhs, {[nil, {location, like_verb}], [[string_constant, escape]]}} when like_verb in [:like, :ilike] ->
        {:function, to_string(like_verb), [lhs, {:like_pattern, string_constant, escape}], location}

      {lhs, {[{location_not, :not}, {location_like, like_verb}], [[string_constant, escape]]}}
      when like_verb in [:like, :ilike] ->
        {:function, "not",
         [{:function, to_string(like_verb), [lhs, {:like_pattern, string_constant, escape}], location_like}],
         location_not}

      {lhs, {[nil, {location, :in}], [in_values]}} ->
        {:function, "in", [lhs | in_values], location}

      {lhs, {[{location_not, :not}, {location_in, :in}], [in_values]}} ->
        {:function, "not", [{:function, "in", [lhs | in_values], location_in}], location_not}

      {lhs, {[location, :is, nil], [:null]}} ->
        {:function, "is_null", [lhs], location}

      {lhs, {[location_is, :is, {location_not, :not}], [:null]}} ->
        {:function, "not", [{:function, "is_null", [lhs], location_is}], location_not}

      {lhs, {[location, nil, :between], [{min, max}]}} ->
        {:function, "and",
         [
           {:function, ">=", [lhs, min], location},
           {:function, "<", [lhs, max], location}
         ], location}

      {lhs, {[location, :not, :between], [{min, max}]}} ->
        {:function, "or",
         [
           {:function, "<", [lhs, min], location},
           {:function, ">=", [lhs, max], location}
         ], location}

      {lhs, {[location, comparator], [rhs]}} ->
        {:function, to_string(comparator), [lhs, rhs], location}

      {lhs, nil} ->
        lhs
    end)
  end

  defp additive_expression() do
    left_associative_expression(keyword_of([:+, :-]), multiplicative_expression())
  end

  defp multiplicative_expression() do
    left_associative_expression(keyword_of([:*, :/, :%]), exponentiation_expression())
  end

  defp exponentiation_expression() do
    left_associative_expression(keyword(:^), unary_expression())
  end

  defp unary_expression() do
    choice_deepest_error([
      sequence([keyword(:+), concat_expression()]),
      sequence([next_position(), keyword(:-), concat_expression()]),
      concat_expression()
    ])
    |> map(fn
      [location, :-, inner] ->
        {:function, "-", [{:constant, :integer, 0, _location = nil}, inner], location}

      [:+, inner] ->
        inner

      other ->
        other
    end)
  end

  defp concat_expression() do
    left_associative_expression(keyword(:||), infix_cast_expression())
    |> map(&normalize_concat/1)
  end

  defp normalize_concat({:function, "||", args, location}),
    do: {:function, "concat", Enum.map(args, &normalize_concat/1), location}

  defp normalize_concat(other), do: other

  defp infix_cast_expression(),
    do:
      sequence([parenthesised_expression(), option(cast_suffix())])
      |> map(fn [expr, cast_suffix] -> build_cast(expr, cast_suffix) end)

  defp cast_suffix(),
    do: sequence([next_position(), keyword(:"::"), data_type(), lazy(fn -> option(cast_suffix()) end)])

  defp build_cast(expr, nil), do: expr

  defp build_cast(expr, [location, :"::", type, next_cast]),
    do: build_cast({:function, {:cast, type}, [expr], location}, next_cast)

  defp parenthesised_expression(), do: paren_parser(column(), simple_expression())

  defp simple_expression() do
    choice_deepest_error([
      cast_expression(),
      bucket_expression(),
      function_expression(),
      extract_expression(),
      trim_expression(),
      case_expression(),
      substring_expression(),
      null_expression(),
      constant_column(),
      field_or_parameter() |> label("column definition")
    ])
  end

  defp null_expression(), do: keyword(:null)

  defp constant_column() do
    either_deepest_error(typed_literal(), any_constant())
  end

  defp select_column(), do: choice_deepest_error([select_all(), select_all_from_table(), plain_select_column()])

  defp select_all(), do: pipe([next_position(), keyword(:*)], fn [location, _] -> {:*, location} end)

  defp select_all_from_table() do
    pipe([next_position(), identifier(), keyword(:.), keyword(:*)], fn [location, {_type, table_name}, :., :*] ->
      {{:*, table_name}, location}
    end)
  end

  defp plain_select_column() do
    pipe(
      [
        column(),
        option(
          keyword(:as)
          |> name()
        )
      ],
      fn
        [column, nil] -> column
        [column, :as, name] -> {column, :as, name}
      end
    )
  end

  defp name(parser) do
    map(
      parser,
      pair_both(
        identifier(),
        many(
          pair_both(
            keyword(:.),
            identifier()
          )
        )
      ),
      fn {{_, first}, rest} ->
        [first | Enum.map(rest, fn {:., {_, part}} -> part end)] |> Enum.join(".")
      end
    )
  end

  defp cast_expression() do
    pipe(
      [
        next_position(),
        keyword(:cast),
        keyword(:"("),
        column(),
        either_deepest_error(keyword(:","), keyword(:as)),
        data_type(),
        keyword(:")")
      ],
      fn [location, :cast, :"(", expr, _, type, :")"] ->
        {:function, {:cast, type}, [expr], location}
      end
    )
  end

  defp data_type() do
    choice_deepest_error([
      raw_identifier_of(~w(integer real float text boolean datetime timestamp date time interval)),
      sequence([raw_identifier("double"), raw_identifier("precision")])
    ])
    |> map(fn
      :float -> :real
      :timestamp -> :datetime
      [:double, :precision] -> :real
      other -> other
    end)
    |> label("type name")
  end

  defp bucket_expression() do
    pipe(
      [
        next_position(),
        keyword(:bucket),
        keyword(:"("),
        column(),
        keyword(:by),
        numeric_constant(),
        option(
          sequence([
            keyword(:align),
            align_type()
          ])
        ),
        keyword(:")")
      ],
      fn
        [location, :bucket, :"(", arg1, :by, arg2, [:align, type], :")"] ->
          {:function, {:bucket, type}, [arg1, arg2], location}

        [location, :bucket, :"(", arg1, :by, arg2, nil, :")"] ->
          {:function, {:bucket, :lower}, [arg1, arg2], location}
      end
    )
  end

  defp align_type() do
    raw_identifier_of(~w(bottom lower top upper middle))
    |> map(fn
      :bottom -> :lower
      :top -> :upper
      x -> x
    end)
  end

  defp raw_identifier(word), do: raw_identifier_of([word])

  defp raw_identifier_of(words) do
    unquoted_identifier()
    |> map(&String.downcase/1)
    |> satisfy(&Enum.member?(words, &1))
    |> map(&String.to_atom/1)
    |> label("one of #{words |> Enum.join(", ")}")
  end

  defp function_expression() do
    switch([
      {next_position() |> function_name() |> keyword(:"("), function_arguments() |> keyword(:")")},
      {:else, error_message(fail(""), "Expected an argument list")}
    ])
    |> map(fn {[location, function, :"("], [arguments, :")"]} ->
      {:function, String.downcase(function), arguments, location}
    end)
  end

  defp function_name(previous_parser) do
    previous_parser
    |> choice_deepest_error([
      unquoted_identifier(),
      keyword(:left),
      keyword(:right)
    ])
    |> map(&to_string/1)
  end

  defp function_arguments() do
    choice_deepest_error([
      comma_delimited1(column()),
      distinct_identifier(),
      all_identifier(),
      keyword(:*)
    ])
    |> option()
    |> map(fn
      [_ | _] = arguments -> arguments
      nil -> []
      single_argument -> [single_argument]
    end)
  end

  defp extract_expression() do
    pipe(
      [
        next_position(),
        keyword(:extract),
        keyword(:"("),
        date_part(),
        keyword(:from),
        map(column(), &[&1]),
        keyword(:")")
      ],
      fn [location, :extract, :"(", part, :from, column, :")"] ->
        {:function, to_string(part), column, location}
      end
    )
  end

  defp date_part(),
    do:
      raw_identifier_of(~w(hour minute second year quarter month day weekday dow))
      |> label("date part")

  defp trim_expression() do
    pipe(
      [
        next_position(),
        keyword(:trim),
        keyword(:"("),
        option(choice_deepest_error([keyword(:both), keyword(:leading), keyword(:trailing)])),
        option(constant(:string)),
        option(keyword(:from)),
        column(),
        keyword(:")")
      ],
      fn
        [location, :trim, :"(", trim_type, nil, _, column, :")"] ->
          {:function, trim_function(trim_type), [column], location}

        [location, :trim, :"(", trim_type, chars, _, column, :")"] ->
          {:function, trim_function(trim_type), [column, chars], location}
      end
    )
  end

  defp trim_function(nil), do: "btrim"
  defp trim_function(:both), do: "btrim"
  defp trim_function(:leading), do: "ltrim"
  defp trim_function(:trailing), do: "rtrim"

  defp case_expression() do
    pipe(
      [
        next_position(),
        keyword(:case),
        many1(sequence([keyword(:when), column(), keyword(:then), column()])),
        option(sequence([keyword(:else), column()])),
        keyword(:end)
      ],
      fn
        [location, :case, when_branches, else_branch, :end] ->
          when_args =
            Enum.flat_map(when_branches, fn [:when, condition, :then, expression] -> [condition, expression] end)

          else_arg =
            case else_branch do
              [:else, expression] -> expression
              nil -> :null
            end

          {:function, "case", when_args ++ [else_arg], location}
      end
    )
  end

  defp substring_expression() do
    pipe(
      [
        next_position(),
        either(keyword(:substring), raw_identifier("substr")),
        keyword(:"("),
        column(),
        choice_deepest_error([
          sequence([keyword(:from), pos_integer(), keyword(:for), pos_integer()]),
          sequence([keyword(:","), pos_integer(), keyword(:","), pos_integer()]),
          sequence([
            keyword_of([:from, :for, :","]) |> label("substring arguments"),
            pos_integer()
          ])
        ]),
        keyword(:")")
      ],
      fn
        [location, _, :"(", column, [_, from, _, for_count], :")"] ->
          {:function, "substring", [column, from, for_count], location}

        [location, _, :"(", column, [:for, for_count], :")"] ->
          {:function, "substring", [column, {:constant, :integer, 1, _location = nil}, for_count], location}

        [location, _, :"(", column, [_, from], :")"] ->
          {:function, "substring", [column, from], location}
      end
    )
  end

  defp left_associative_expression(operator, term_parser) do
    sep_by1_eager(term_parser, pair_both(next_position(), operator))
    |> map(fn [first | rest] ->
      rest
      |> Enum.chunk_every(2)
      |> Enum.reduce(first, fn [{location, operator}, right], left ->
        {:function, to_string(operator), [left, right], location}
      end)
    end)
  end

  defp field_or_parameter(), do: either_deepest_error(column_identifier_with_location(), parameter())

  defp column_identifier_with_location() do
    map(
      pair_both(next_position(), column_identifier()),
      fn {location, {table, column}} -> {:identifier, table, column, location} end
    )
  end

  defp column_identifier() do
    choice_deepest_error([
      column_qualified_with_public_schema(),
      qualified_column_with_period_characters(),
      qualified_column(),
      unqualified_column()
    ])
  end

  defp unqualified_column(), do: map(identifier(), &{:unknown, &1})

  defp column_qualified_with_public_schema() do
    # We're ignoring `public.` schema prefix. This is done to support queries issued by psql clients, which might
    # use `public.table.column` to reference columns.
    # The resolving of public prefix is done in the parser to avoid ambiguity which can happen because `foo.bar.baz`
    # also has to be supported (table foo, column bar.baz).
    map(
      sequence([
        ignore(satisfy(identifier(), &match?({_, "public"}, &1))),
        ignore(keyword(:.)),
        either_deepest_error(qualified_column_with_period_characters(), qualified_column())
      ]),
      fn [column] -> column end
    )
  end

  defp qualified_column(), do: map(sequence([identifier(), ignore(keyword(:.)), identifier()]), &List.to_tuple/1)

  defp qualified_column_with_period_characters() do
    map(
      sequence([
        identifier(),
        ignore(keyword(:.)),
        unquoted_identifier(),
        many1(pair_both(keyword(:.), unquoted_identifier()))
      ]),
      fn [table, first_part, other_parts] ->
        column_name = [{:., first_part} | other_parts] |> Enum.map(fn {:., part} -> part end) |> Enum.join(".")
        {table, {:unquoted, column_name}}
      end
    )
  end

  defp distinct_identifier(), do: pair_both(keyword(:distinct), column())

  defp all_identifier(), do: pair_right(keyword(:all), column())

  defp from(), do: from_expression() |> map(&{:from, &1})

  defp from_expression() do
    map(
      comma_delimited1(join_expression() |> map(&join_ast/1)),
      &cross_joins/1
    )
  end

  defp join_ast(join_clauses), do: pair_joins(flatten_join_clauses(join_clauses))

  defp flatten_join_clauses([table]), do: [table]

  defp flatten_join_clauses([table, {join_type, next_join}]) do
    [table, join_type] ++ flatten_join_clauses(next_join)
  end

  defp pair_joins([table_or_join]), do: table_or_join

  defp pair_joins([table_or_join, join_type, table | rest]) do
    pair_joins([to_join(join_type, table_or_join, table) | rest])
  end

  defp to_join({join_type, :on, condition}, left_expr, right_expr),
    do: {:join, %{type: join_type, lhs: left_expr, rhs: right_expr, condition: condition}}

  defp to_join(:cross_join, left_expr, right_expr), do: to_join({:cross_join, :on, nil}, left_expr, right_expr)

  defp cross_joins([table]), do: table
  defp cross_joins([clause | rest]), do: to_join(:cross_join, clause, cross_joins(rest))

  defp join_expression() do
    lazy(fn ->
      sequence([
        table_or_subquery(),
        next_join()
      ])
    end)
  end

  defp join_expression_with_on_clause() do
    lazy(fn ->
      sequence([
        table_or_subquery(),
        keyword(:on),
        column(),
        next_join()
      ])
    end)
  end

  defp next_join() do
    switch([
      {cross_join(), join_expression()},
      {either_deepest_error(inner_join(), outer_join()), join_expression_with_on_clause()},
      {:else, noop()}
    ])
    |> map(&join_clause/1)
  end

  defp join_clause({[join_type], [[rhs, :on, comparison | next_join]]}),
    do: {{join_type, :on, comparison}, [rhs | next_join]}

  defp join_clause({[join_type], [rhs]}), do: {join_type, rhs}

  defp cross_join(), do: replace(pair_both(keyword(:cross), keyword(:join)), :cross_join)

  defp inner_join(), do: replace(pair_both(option(keyword(:inner)), keyword(:join)), :inner_join)

  defp outer_join(), do: choice_deepest_error([outer_join(:left), outer_join(:right), outer_join(:full)])

  defp outer_join(type) do
    sequence([keyword(type), option(keyword(:outer)), keyword(:join)])
    |> replace(:"#{type}_outer_join")
  end

  defp table_or_subquery() do
    switch([
      {keyword_with_position(:"(") |> map(fn {location, _} -> {location, :subquery} end), subquery()},
      {:else, table_name()}
    ])
    |> map(fn
      {[{location, :subquery}], [{:subquery, subquery_data}]} -> {:subquery, postprocess_range(subquery_data, location)}
      other -> other
    end)
  end

  defp postprocess_range(data, start_location) do
    end_location = data.ast.end_location

    update_in(data.ast, fn ast ->
      ast
      |> Map.delete(:end_location)
      |> Map.put(:source_range, {start_location, end_location})
    end)
  end

  defp table_name() do
    sequence([
      either_deepest_error(
        table_with_schema(),
        identifier() |> label("table name")
      ),
      option(sequence([option(keyword(:as)), identifier()]))
    ])
    |> map(fn
      [name, nil] -> name
      [name, [_, {_type, alias}]] -> {name, :as, alias}
    end)
  end

  defp subquery() do
    sequence([
      parenthesised_select_statement(),
      ignore(subquery_close_paren()),
      option(keyword(:as)),
      label(identifier() |> map(fn {_type, value} -> value end), "subquery alias"),
      position()
    ])
    |> map(fn [select_statement, _as_keyword, alias, pos] ->
      {:subquery, %{ast: statement_map([{:end_location, pos} | select_statement]), alias: alias}}
    end)
  end

  defp subquery_close_paren(),
    do: keyword(:")") |> error_message("Unexpected input after end of valid subquery, expected `)`")

  defp table_with_schema() do
    pipe([identifier(), keyword(:.), identifier()], fn
      [{:unquoted, schema}, _, {:unquoted, table}] -> {:unquoted, "#{schema}.#{table}"}
      [{_, schema}, _, {_, table}] -> {:quoted, "#{schema}.#{table}"}
    end)
  end

  defp optional_where() do
    switch([
      {keyword(:where), column()},
      {:else, noop()}
    ])
    |> map(fn
      {[:where], [expression]} -> {:where, expression}
      other -> other
    end)
  end

  defp like_escape() do
    option(sequence([keyword(:escape), constant(:string)]))
    |> map(fn
      [_, constant] -> constant
      nil -> {:constant, :string, nil, _location = nil}
    end)
  end

  defp comparison_range() do
    pipe([comparison_value(), keyword(:and), comparison_value()], fn [min, :and, max] -> {min, max} end)
  end

  defp comparison_value() do
    either_deepest_error(additive_expression(), any_constant())
    |> label("comparison value")
  end

  defp in_values() do
    pipe([keyword(:"("), comma_delimited1(additive_expression()), keyword(:")")], fn [_, values, _] ->
      values
    end)
  end

  defp typed_literal() do
    pipe(
      [
        raw_identifier_of(~w(datetime timestamp date time interval)),
        constant(:string)
      ],
      fn
        [:timestamp, {:constant, :string, value, location}] -> {:constant, :datetime, value, location}
        [type, {:constant, :string, value, location}] -> {:constant, type, value, location}
      end
    )
    |> label("typed literal")
  end

  defp any_constant() do
    either_deepest_error(
      constant_of([:string, :boolean]),
      numeric_constant()
    )
    |> label("constant")
  end

  defp numeric_constant(expected_type \\ :numeric) do
    # Note that we're resolving explicit sign prefix in the parser and not in the lexer.
    # Resolving this in the lexer means that e.g. 1+2 would generate tokens `constant(1)` and `constant(2)`,
    # instead of `constant(1)`, `keyword(+)`, `constant(2)`. In other words, we would require whitespace
    # after the `+` and `-` characters for them to work as binary operators.
    map(sequence([option(keyword_of([:+, :-])), constant_of([:integer, :float])]), fn
      [nil, {:constant, _type, _value, _location} = constant] -> constant
      [:+, {:constant, _type, _value, _location} = constant] -> constant
      [:-, {:constant, type, value, location}] -> {:constant, type, value * -1, location}
    end)
    |> satisfy(fn {:constant, type, _value, _location} ->
      expected_type == :numeric || type == expected_type
    end)
    |> label("#{expected_type} constant")
  end

  defp non_neg_integer(),
    do:
      numeric_constant(:integer)
      |> satisfy(fn {:constant, :integer, value, _location} -> value >= 0 end)
      |> label("non-negative integer constant")

  defp pos_integer(),
    do:
      numeric_constant(:integer)
      |> satisfy(fn {:constant, :integer, value, _location} -> value > 0 end)
      |> label("positive integer constant")

  defp constant_of(expected_types) do
    choice_deepest_error(Enum.map(expected_types, &constant/1))
    |> label(expected_types |> Enum.map(&"#{&1} constant") |> Enum.join(" or "))
  end

  defp constant(expected_type) do
    token(:constant)
    |> satisfy(fn token -> token.value.type == expected_type end)
    |> map(&{:constant, &1.value.type, &1.value.value, {&1.line, &1.column + 1}})
    |> label("#{expected_type} constant")
  end

  defp optional_order_by() do
    switch([
      {keyword(:order), pair_both(keyword(:by), comma_delimited1(order_by_field()))},
      {:else, noop()}
    ])
    |> map(fn {[:order], [{:by, fields}]} -> {:order_by, fields} end)
  end

  defp optional_group_by() do
    switch([
      {keyword(:group), pair_both(keyword(:by), grouping_sets())},
      {:else, noop()}
    ])
    |> map(fn {[:group], [{:by, grouping_sets}]} -> {:grouping_sets, grouping_sets} end)
  end

  defp grouping_sets() do
    switch([
      {keyword(:grouping), sequence([keyword(:sets), keyword(:"("), any_grouping_sets(), keyword(:")")])},
      {keyword(:rollup), sequence([keyword(:"("), non_empty_grouping_sets(), keyword(:")")])},
      {keyword(:cube), sequence([keyword(:"("), non_empty_grouping_sets(), keyword(:")")])},
      {:else, any_grouping_sets()}
    ])
    |> map(fn
      {[:grouping], [[:sets, :"(", grouping_sets, :")"]]} -> grouping_sets
      {[:rollup], [[:"(", grouping_sets, :")"]]} -> sets_rollup(grouping_sets)
      {[:cube], [[:"(", grouping_sets, :")"]]} -> grouping_sets |> sets_cube() |> Enum.sort_by(&length/1, &>=/2)
      grouping_set -> [Enum.flat_map(grouping_set, &List.wrap/1)]
    end)
  end

  defp any_grouping_sets() do
    comma_delimited1(
      switch([
        {keyword(:"("), pair_both(option(comma_delimited1(column())), keyword(:")"))},
        {:else, column()}
      ])
      |> map(fn
        {[:"("], [{nil, :")"}]} -> []
        {[:"("], [{group, :")"}]} -> group
        column -> [column]
      end)
    )
  end

  defp non_empty_grouping_sets() do
    comma_delimited1(
      switch([
        {keyword(:"("), pair_both(comma_delimited1(column()), keyword(:")"))},
        {:else, column()}
      ])
      |> map(fn
        {[:"("], [{group, :")"}]} -> group
        column -> [column]
      end)
    )
  end

  defp sets_rollup(sets), do: Enum.map(length(sets)..0, &(sets |> Enum.take(&1) |> List.flatten()))

  defp sets_cube([]), do: [[]]

  defp sets_cube([head | tail]) do
    tail_cube = sets_cube(tail)
    Enum.map(tail_cube, &(head ++ &1)) ++ tail_cube
  end

  defp order_by_field() do
    sequence([column(), order_by_direction(), nulls_specifier()]) |> map(&List.to_tuple/1)
  end

  defp order_by_direction(), do: choice_deepest_error([keyword(:asc), keyword(:desc), return(:asc)])

  defp nulls_specifier(),
    do:
      switch([
        {keyword(:nulls), raw_identifier_of(~w(first last))},
        {:else, return(:nulls_natural)}
      ])
      |> map(fn
        {[:nulls], [:first]} -> :nulls_first
        {[:nulls], [:last]} -> :nulls_last
        other -> other
      end)

  defp identifier(parser \\ noop()) do
    parser
    |> either_deepest_error(token(:quoted), token(:unquoted))
    |> map(&{&1.category, &1.value})
    |> label("identifier")
  end

  defp unquoted_identifier(parser \\ noop()) do
    parser
    |> token(:unquoted)
    |> map(& &1.value)
    |> label("identifier")
  end

  defp keyword_of(parser \\ noop(), types) do
    parser
    |> choice_deepest_error(Enum.map(types, &keyword(&1)))
    |> label(types |> Enum.join(" or "))
  end

  defp keyword_of_with_position(parser, types), do: pair_both(parser, next_position(), keyword_of(types))

  defp keyword(parser \\ noop(), type) do
    parser
    |> token(type)
    |> map(& &1.category)
    |> label(to_string(type))
  end

  defp keyword_with_position(parser \\ noop(), type), do: pair_both(parser, next_position(), keyword(type))

  defp comma_delimited(term_parser) do
    sep_by_until(term_parser, keyword(:","), keyword(:from))
    |> map(fn
      [] -> []
      [first | rest] -> [first | Enum.drop_every(rest, 2)]
    end)
  end

  defp comma_delimited1(term_parser) do
    sep_by1_eager(term_parser, keyword(:","))
    |> map(fn [first | rest] -> [first | Enum.drop_every(rest, 2)] end)
  end

  defp end_of_input(parser) do
    parser
    |> error_message(token(:eof), "Unexpected input after end of valid query")
    |> ignore()
  end

  defp replace(parser, value), do: map(parser, fn _ -> value end)

  defp optional_limit() do
    switch([
      {keyword(:limit), pos_integer()},
      {:else, noop()}
    ])
    |> map(fn {[:limit], [{:constant, :integer, amount, _}]} -> {:limit, amount} end)
  end

  defp optional_offset() do
    switch([
      {keyword(:offset), non_neg_integer()},
      {:else, noop()}
    ])
    |> map(fn {[:offset], [{:constant, :integer, amount, _}]} -> {:offset, amount} end)
  end

  defp optional_having() do
    switch([
      {keyword(:having), column()},
      {:else, noop()}
    ])
    |> map(fn
      {[:having], [expression]} -> {:having, expression}
      other -> other
    end)
  end

  defp disjunction_expression(term_parser),
    do:
      left_associative_expression(
        keyword(:or),
        conjunction_expression(term_parser)
      )

  defp conjunction_expression(term_parser),
    do:
      left_associative_expression(
        keyword(:and),
        not_expression(term_parser)
      )

  defp not_expression(term_parser),
    do:
      choice_deepest_error([
        sequence([keyword_with_position(:not), lazy(fn -> not_expression(term_parser) end)]),
        term_parser,
        paren_parser(column(), term_parser)
      ])
      |> map(fn
        [{location, :not}, expr] -> {:function, "not", [expr], location}
        expr -> expr
      end)

  defp paren_parser(in_parens_parser, term_parser),
    do:
      switch([
        {keyword(:"("), in_parens_parser |> keyword(:")")},
        {:else, term_parser}
      ])
      |> map(fn
        {[:"("], [result, :")"]} -> result
        result -> result
      end)

  defp optional_distinct() do
    either_deepest_error(keyword(:distinct), keyword(:all))
    |> option()
    |> map(fn
      :distinct -> {:distinct?, true}
      _ -> {:distinct?, false}
    end)
  end

  defp parameter() do
    token(:parameter)
    |> map(&{:parameter, &1.value})
    |> label("expected parameter")
  end

  defp next_position(),
    do:
      position()
      |> map(fn {line, column} -> {line, column + 1} end)

  # -------------------------------------------------------------------
  # For tests
  # -------------------------------------------------------------------

  if Mix.env() == :test do
    def parse!(statement) do
      {:ok, ast} = parse(statement)
      ast
    end
  end
end
