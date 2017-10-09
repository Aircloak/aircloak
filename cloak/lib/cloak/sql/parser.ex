defmodule Cloak.Sql.Parser do
  @moduledoc "Parser for SQL queries."
  import Combine.Parsers.Base
  import Cloak.Sql.Parsers

  @type comparator ::
      :=
    | :<
    | :<=
    | :>=
    | :>
    | :<>

  @type unqualified_identifier :: {:quoted, String.t} | {:unquoted, String.t}

  @type qualified_identifier :: {:identifier, :unknown | table, unqualified_identifier}

  @type data_type :: Cloak.DataSource.data_type | :interval

  @type function_name :: String.t | {:bucket, atom} | {:cast, data_type}

  @type constant :: {:constant, data_type, any}

  @type column ::
      qualified_identifier
    | {:distinct, qualified_identifier}
    | function_spec
    | constant
    | {:parameter, pos_integer}

  @type function_spec :: {:function, function_name, [column]}

  @type comparison :: {:comparison, column, comparator, any}

  @type condition ::
      comparison
    | {:like | :ilike, column, constant, constant}
    | {:is, String.t, :null}
    | {:in, String.t, [any]}

  @type where_clause ::
      nil
    | condition
    | {:not, condition}
    | {:and | :or, condition, condition}

  @type having_clause ::
      nil
    | comparison
    | {:not, comparison}
    | {:and | :or, comparison, comparison}

  @type from_clause :: table | subquery | join

  @type table :: unqualified_identifier | {unqualified_identifier, :as, String.t}

  @type join ::
    {:join, %{
      type: :cross_join | :inner_join | :full_outer_join | :left_outer_join | :right_outer_join,
      lhs: from_clause,
      rhs: from_clause,
      conditions: where_clause
    }}

  @type subquery :: {:subquery, %{ast: parsed_query, alias: String.t}}

  @type parsed_query :: %{
    command: :select | :show,
    columns: [column | {column, :as, String.t} | {:*, String.t} | :*],
    group_by: [String.t],
    from: from_clause,
    where: where_clause,
    order_by: [{String.t, :asc | :desc}],
    having: having_clause,
    show: :tables | :columns,
    limit: integer,
    offset: integer,
    distinct?: boolean,
    subquery?: boolean,
    sample_rate: integer,
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Parses a SQL query in text form. Raises on error."
  @spec parse!(String.t) :: parsed_query
  def parse!(string) do
    {:ok, query} = parse(string)
    query
  end

  @doc "Parses a SQL query in text form."
  @spec parse(String.t) :: {:ok, parsed_query} | {:error, any}
  def parse(string) do
    with {:ok, tokens} <- Cloak.Sql.Lexer.tokenize(string) do
      case Combine.parse(tokens, parser()) do
        {:error, _} = error -> error
        [statement] -> {:ok, statement}
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
      {keyword(:select), select_statement()},
      {keyword(:show), show_statement()},
      {:else, error_message(fail(""), "Expected `select or show`")}
    ])
    |> map(fn({[command], [statement_data]}) -> statement_map(command, statement_data) end)
  end

  defp statement_map(command, statement_data) do
    statement_data
    |> Enum.reject(fn(value) -> value == nil end)
    |> Enum.into(%{})
    |> Map.put(:command, command)
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
    |> map(fn({[show], data}) -> [{:show, show} | data] end)
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
      optional_offset(),
      optional_sample_users()
    ])
  end

  defp select_columns() do
    map(comma_delimited(select_column()), &{:columns, &1})
  end

  defp column() do
    lazy(fn -> additive_expression() end)
  end

  defp additive_expression() do
    infix_expression([keyword(:+), keyword(:-)], multiplicative_expression())
  end

  defp multiplicative_expression() do
    infix_expression([keyword(:*), keyword(:/), keyword(:%)], exponentiation_expression())
  end

  def exponentiation_expression() do
    infix_expression([keyword(:^)], unary_expression())
  end

  def unary_expression() do
    choice_deepest_error([
      sequence([keyword(:+), concat_expression()]),
      sequence([keyword(:-), concat_expression()]),
      concat_expression()
    ])
    |> map(fn
      [:-, inner] -> {:function, "-", [{:constant, :integer, 0}, inner]}
      [:+, inner] -> inner
      other -> other
    end)
  end

  defp concat_expression() do
    infix_expression([keyword(:||)], infix_cast_expression())
    |> map(&normalize_concat/1)
  end

  defp normalize_concat({:function, "||", args}), do: {:function, "concat", Enum.map(args, &normalize_concat/1)}
  defp normalize_concat(other), do: other

  defp infix_cast_expression() do
    infix_expression([keyword(:"::")], simple_expression(), data_type())
    |> map(fn
      {:function, "::", [expr, type]} -> {:function, {:cast, type}, [expr]}
      other -> other
    end)
  end

  defp simple_expression() do
    choice_deepest_error([
      parenthesised_expression(),
      cast_expression(),
      bucket_expression(),
      function_expression(),
      extract_expression(),
      trim_expression(),
      substring_expression(),
      field_or_parameter(),
      constant_column() |> label("column definition")
    ])
  end

  defp parenthesised_expression() do
    pipe(
      [keyword(:"("), column(), keyword(:")")],
      fn([:"(", result, :")"]) -> result end
    )
  end

  defp constant_column() do
    either_deepest_error(interval(), any_constant())
  end

  defp select_column(), do:
    choice_deepest_error([keyword(:*), select_all_from_table(), plain_select_column()])

  defp select_all_from_table(), do:
    pipe(
      [identifier(), keyword(:.), keyword(:*)],
      fn([{_type, table_name}, :., :*]) -> {:*, table_name} end
    )

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
        ([column, nil]) -> column
        ([column, :as, name]) -> {column, :as, name}
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
            keyword(:"."),
            identifier()
          )
        )
      ),
      fn ({{_, first}, rest}) ->
          [first | Enum.map(rest, fn ({:., {_, part}}) -> part end)] |> Enum.join(".")
      end
    )
  end

  defp cast_expression() do
    pipe(
      [
        keyword(:cast),
        keyword(:"("),
        column(),
        either(keyword(:","), keyword(:as)),
        data_type(),
        keyword(:")"),
      ],
      fn [:cast, :"(", expr, _, type, :")"] -> {:function, {:cast, type}, [expr]} end
    )
  end

  defp data_type() do
    choice([
      raw_identifier_of(~w(integer real float text boolean datetime timestamp date time)),
      sequence([raw_identifier("double"), raw_identifier("precision")]),
      keyword(:interval)
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
        keyword(:bucket),
        keyword(:"("),
        column(),
        keyword(:by),
        numeric_constant(),
        option(sequence([
          keyword(:align),
          align_type(),
        ])),
        keyword(:")"),
      ],
      fn
        [:bucket, :"(", arg1, :by, arg2, [:align, type], :")"] -> {:function, {:bucket, type}, [arg1, arg2]}
        [:bucket, :"(", arg1, :by, arg2, nil, :")"] -> {:function, {:bucket, :lower}, [arg1, arg2]}
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

  defp raw_identifier(word), do:
    raw_identifier_of([word])

  defp raw_identifier_of(words) do
    unquoted_identifier()
    |> map(&String.downcase/1)
    |> satisfy(&Enum.member?(words, &1))
    |> map(&String.to_atom/1)
  end

  defp function_expression() do
    switch([
      {function_name() |> keyword(:"("), lazy(fn -> function_arguments() end) |> keyword(:")")},
      {:else, error_message(fail(""), "Expected an argument list")}
    ])
    |> map(fn
      {[function, :"("], [arguments, :")"]} -> {:function, String.downcase(function), arguments}
    end)
  end

  defp function_name() do
    choice([
      unquoted_identifier(),
      keyword(:left),
      keyword(:right),
    ])
    |> map(&to_string/1)
  end

  defp function_arguments() do
    choice_deepest_error([
      comma_delimited(column()),
      distinct_identifier(),
      keyword(:*)
    ])
    |> map(fn
      [_|_] = arguments -> arguments
      single_argument -> [single_argument]
    end)
  end

  defp extract_expression() do
    pipe(
      [
        keyword(:extract),
        keyword(:"("),
        unquoted_identifier(),
        keyword(:from),
        lazy(fn -> map(column(), &[&1]) end),
        keyword(:")"),
      ],
      fn([:extract, :"(", part, :from, column, :")"]) -> {:function, String.downcase(part), column} end
    )
  end

  defp trim_expression() do
    pipe(
      [
        keyword(:trim),
        keyword(:"("),
        option(choice([keyword(:both), keyword(:leading), keyword(:trailing)])),
        option(constant(:string)),
        option(keyword(:from)),
        column(),
        keyword(:")"),
     ],
     fn
       [:trim, :"(", trim_type, nil, _, column, :")"] ->
         {:function, trim_function(trim_type), [column]}
       [:trim, :"(", trim_type, chars, _, column, :")"] ->
         {:function, trim_function(trim_type), [column, chars]}
     end
   )
  end

  defp trim_function(nil), do: "btrim"
  defp trim_function(:both), do: "btrim"
  defp trim_function(:leading), do: "ltrim"
  defp trim_function(:trailing), do: "rtrim"

  defp substring_expression() do
    pipe(
      [
        keyword(:substring),
        keyword(:"("),
        column(),
        choice_deepest_error([
          sequence([keyword(:from), pos_integer(), keyword(:for), pos_integer()]),
          sequence([keyword(:","), pos_integer(), keyword(:","), pos_integer()]),
          sequence([keyword_of([:from, :for, :","]), pos_integer()])
        ]),
        keyword(:")"),
     ],
     fn
       [:substring, :"(", column, [_, from, _, for_count], :")"] ->
         {:function, "substring", [column, from, for_count]}
       [:substring, :"(", column, [:for, for_count], :")"] ->
         {:function, "substring", [column, {:constant, :integer, 1}, for_count]}
       [:substring, :"(", column, [_, from], :")"] ->
         {:function, "substring", [column, from]}
     end
   )
  end

  defp infix_expression(operators, left_parser, right_parser \\ nil) do
    pipe(
      [
        left_parser,
        many(sequence([choice(operators), right_parser || left_parser])),
      ],
      fn[first, rest] -> Enum.reduce(rest, first,
        fn([operator, right], left) -> {:function, to_string(operator), [left, right]} end)
      end
    )
  end

  defp field_or_parameter(), do:
    either(qualified_identifier(), parameter())

  defp qualified_identifier() do
    map(
      pair_both(
        identifier(),
        many(
          pair_both(
            keyword(:"."),
            identifier()
          )
        )
      ),
      fn
        ({column, []}) ->
          {:identifier, :unknown, column}
        ({table, [{:., column}]}) ->
          {:identifier, table, column}
        ({table, parts}) ->
          column = parts |> Enum.map(fn ({:., {:unquoted, part}}) -> part end) |> Enum.join(".")
          {:identifier, table, {:unquoted, column}}
      end
    )
  end

  defp distinct_identifier() do
    pair_both(keyword(:distinct), column())
  end

  defp from() do
    pair_both(
      keyword(:from),
      from_expression()
    )
  end

  defp from_expression() do
    map(
      comma_delimited(join_expression() |> map(&join_ast/1)),
      &cross_joins/1
    )
  end

  defp join_ast(join_clauses),
    do: pair_joins(flatten_join_clauses(join_clauses))

  defp flatten_join_clauses([table]), do: [table]
  defp flatten_join_clauses([table, {join_type, next_join}]) do
    [table, join_type] ++ flatten_join_clauses(next_join)
  end

  defp pair_joins([table_or_join]), do: table_or_join
  defp pair_joins([table_or_join, join_type, table | rest]) do
    pair_joins([to_join(join_type, table_or_join, table) | rest])
  end

  defp to_join({join_type, :on, conditions}, left_expr, right_expr),
    do: {:join, %{type: join_type, lhs: left_expr, rhs: right_expr, conditions: conditions}}
  defp to_join(:cross_join, left_expr, right_expr),
    do: to_join({:cross_join, :on, nil}, left_expr, right_expr)

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
        where_expressions(),
        next_join()
      ])
    end)
  end

  defp next_join() do
    switch([
      {cross_join(),join_expression()},
      {either_deepest_error(inner_join(), outer_join()), join_expression_with_on_clause()},
      {:else, noop()}
    ])
    |> map(&join_clause/1)
  end

  defp join_clause({[join_type], [[rhs, :on, comparison | next_join]]}),
    do: {{join_type, :on, comparison}, [rhs | next_join]}
  defp join_clause({[join_type], [rhs]}),
    do: {join_type, rhs}

  defp cross_join(),
    do: replace(pair_both(keyword(:cross), keyword(:join)), :cross_join)

  defp inner_join(),
    do: replace(pair_both(option(keyword(:inner)), keyword(:join)), :inner_join)

  defp outer_join(),
    do: choice([outer_join(:left), outer_join(:right), outer_join(:full)])

  defp outer_join(type) do
    sequence([keyword(type), option(keyword(:outer)), keyword(:join)])
    |> replace(:"#{type}_outer_join")
  end

  defp table_or_subquery() do
    switch([
      {keyword(:"(") |> map(fn(_) -> :subquery end), subquery()},
      {:else, table_name()}
    ])
    |> map(fn
      {[:subquery], [subquery_data]} -> {:subquery, subquery_data}
      other -> other
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
      ignore(keyword(:select)),
      lazy(fn -> select_statement() end),
      ignore(keyword(:")")),
      option(keyword(:as)),
      label(identifier() |> map(fn({_type, value}) -> value end), "subquery alias")
    ])
    |> map(fn([select_statement, _as_keyword, alias]) ->
      %{ast: statement_map(:select, select_statement), alias: alias}
    end)
  end

  defp table_with_schema() do
    pipe([identifier(), keyword(:.), identifier()], fn
      [{:unquoted, schema}, _, {:unquoted, table}] -> {:unquoted, "#{schema}.#{table}"}
      [{_, schema}, _, {_, table}] -> {:quoted, "#{schema}.#{table}"}
    end)
  end

  defp optional_where() do
    switch([
      {keyword(:where), where_expressions()},
      {:else, noop()}
    ])
    |> map(fn
          {[:where], [where_expressions]} -> {:where, where_expressions}
          other -> other
        end)
  end

  defp where_expressions(), do: conjunction_expression(where_expression(), "Invalid where expression.")

  defp where_expression() do
    switch([
      {column() |> option(keyword(:not)) |> choice([keyword(:like), keyword(:ilike)]),
        sequence([constant(:string), like_escape()])},
      {column() |> option(keyword(:not)) |> keyword(:in), in_values()},
      {column() |> keyword(:is) |> option(keyword(:not)), keyword(:null)},
      {column() |> keyword(:between), allowed_where_range()},
      {any_constant() |> inequality_comparator(), column()},
      {column() |> inequality_comparator(), any_constant()},
      {column() |> equality_comparator(), allowed_where_value()},
      {:else, error_message(fail(""), "Invalid where expression.")}
    ])
    |> map(fn
          {[identifier, nil, :like], [[string_constant, escape]]} ->
            {:like, identifier, {:like_pattern, string_constant, escape}}
          {[identifier, nil, :ilike], [[string_constant, escape]]} ->
            {:ilike, identifier, {:like_pattern, string_constant, escape}}
          {[identifier, :not, :like], [[string_constant, escape]]} ->
            {:not, {:like, identifier, {:like_pattern, string_constant, escape}}}
          {[identifier, :not, :ilike], [[string_constant, escape]]} ->
            {:not, {:ilike, identifier, {:like_pattern, string_constant, escape}}}
          {[identifier, nil, :in], [in_values]} -> {:in, identifier, in_values}
          {[identifier, :not, :in], [in_values]} -> {:not, {:in, identifier, in_values}}
          {[identifier, :is, nil], [:null]} -> {:is, identifier, :null}
          {[identifier, :is, :not], [:null]} -> {:not, {:is, identifier, :null}}
          {[identifier, :between], [{min, max}]} ->
            {:and, {:comparison, identifier, :>=, min}, {:comparison, identifier, :<, max}}
          {[lhs, comparator], [rhs]} -> create_comparison(lhs, comparator, rhs)
        end)
  end

  defp like_escape() do
    option(sequence([keyword(:escape), constant(:string)]))
    |> map(fn
      [_, constant] -> constant
      nil -> {:constant, :string, nil}
    end)
  end

  defp allowed_where_value() do
    either(column(), any_constant())
    |> label("comparison value")
  end

  defp allowed_where_range() do
    pipe(
      [any_constant(), keyword(:and), any_constant()],
      fn([min, :and, max]) -> {min, max} end
    )
  end

  defp in_values() do
    pipe(
      [keyword(:"("), comma_delimited(any_constant()), keyword(:")")],
      fn([_, values, _]) -> values end
    )
  end

  defp interval() do
    pipe(
      [
        keyword(:interval),
        constant_of([:string]),
      ],
      fn([:interval, {:constant, :string, value}]) -> Timex.Duration.parse(value) end
    )
    |> satisfy(&match?({:ok, _}, &1))
    |> map(fn({:ok, result}) -> {:constant, :interval, result} end)
  end

  defp any_constant() do
    either(
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
    map(
      sequence([option(keyword_of([:+, :-])), constant_of([:integer, :float])]),
      fn
        [nil, {:constant, _type, _value} = constant] -> constant
        [:+, {:constant, _type, _value} = constant] -> constant
        [:-, {:constant, type, value}] -> {:constant, type, value * -1}
      end
    )
    |> satisfy(fn({:constant, type, _value}) -> expected_type == :numeric || type == expected_type end)
    |> label("#{expected_type} constant")
  end

  defp non_neg_integer(), do:
    numeric_constant(:integer)
    |> satisfy(fn({:constant, :integer, value}) -> value >= 0 end)
    |> label("non-negative integer constant")

  defp pos_integer(), do:
    numeric_constant(:integer)
    |> satisfy(fn({:constant, :integer, value}) -> value > 0 end)
    |> label("positive integer constant")

  defp constant_of(expected_types) do
    choice(Enum.map(expected_types, &constant/1))
    |> label(expected_types |> Enum.map(&"#{&1} constant") |> Enum.join(" or "))
  end

  defp constant(expected_type) do
    token(:constant)
    |> satisfy(fn(token) -> token.value.type == expected_type end)
    |> map(&{:constant, &1.value.type, &1.value.value})
    |> label("#{expected_type} constant")
  end

  defp optional_order_by() do
    switch([
      {keyword(:order), pair_both(keyword(:by), comma_delimited(order_by_field()))},
      {:else, noop()}
    ])
    |> map(fn({[:order], [{:by, fields}]}) -> {:order_by, fields} end)
  end

  defp optional_group_by() do
    switch([
      {keyword(:group), pair_both(keyword(:by), comma_delimited(column()))},
      {:else, noop()}
    ])
    |> map(fn {_, [{:by, columns}]} -> {:group_by, columns} end)
  end

  defp order_by_field() do
    pair_both(
      column(),
      order_by_direction()
    )
  end

  defp order_by_direction() do
    option(
      either(
        keyword(:asc),
        keyword(:desc)
      )
    )
  end

  defp identifier(parser \\ noop()) do
    parser
    |> either(token(:quoted), token(:unquoted))
    |> map(&{&1.category, &1.value})
    |> label("identifier")
  end

  defp unquoted_identifier(parser \\ noop()) do
    parser
    |> token(:unquoted)
    |> map(&(&1.value))
    |> label("identifier")
  end

  defp comparator(parser \\ noop()), do:
    parser |> either(equality_comparator(), inequality_comparator()) |> label("comparator")

  defp equality_comparator(parser \\ noop()), do:
    parser |> keyword_of([:=, :<>]) |> label("equality comparator")

  defp inequality_comparator(parser \\ noop()), do:
    parser |> keyword_of([:<, :<=, :>, :>=]) |> label("inequality comparator")

  defp keyword_of(parser \\ noop(), types) do
    parser
    |> choice(Enum.map(types, &keyword(&1)))
    |> label(types |> Enum.join(" or "))
  end

  defp keyword(parser \\ noop(), type) do
    parser
    |> token(type)
    |> map(&(&1.category))
    |> label(to_string(type))
  end

  defp comma_delimited(term_parser) do
    sep_by1_eager(term_parser, keyword(:","))
  end

  defp conjunction_expression_parser(term_parser, error_message) do
    recur = lazy(fn -> conjunction_expression_parser(term_parser, error_message) end)

    choice_deepest_error([
      sequence([term_parser, keyword_of([:and, :or]), recur]),
      sequence([keyword(:"("), recur, keyword(:")"), keyword_of([:and, :or]), recur]),
      sequence([keyword(:"("), recur, keyword(:")")]),
      term_parser,
      error_message(fail(""), error_message),
    ])
    |> map(fn
      [:"(", terms, :")"] -> terms
      [:"(", terms, :")", op, rest] when op in [:and, :or] -> [terms, op | rest]
      [term, op, rest] when op in [:and, :or] -> [term, op | rest]
      term -> [term]
    end)
  end

  defp conjunction_expression(term_parser, error_message), do:
    conjunction_expression_parser(term_parser, error_message)
    |> map(&build_expression_tree/1)

  defp build_expression_tree([term]), do: build_expression_tree(term)
  defp build_expression_tree(terms) when is_list(terms) do
    terms
    |> Enum.split_while(& &1 != :or)
    |> case do
      {[lhs, :and | rhs], []} -> {:and, build_expression_tree(lhs), build_expression_tree(rhs)}
      {lhs, [:or | rhs]} -> {:or, build_expression_tree(lhs), build_expression_tree(rhs)}
    end
  end
  defp build_expression_tree(term), do: term

  defp end_of_input(parser) do
    parser
    |> error_message(token(:eof), "Expected end of input.")
    |> ignore()
  end

  defp replace(parser, value), do: map(parser, fn(_) -> value end)

  defp optional_limit() do
    switch([
      {keyword(:limit), pos_integer()},
      {:else, noop()}
    ])
    |> map(fn {[:limit], [{:constant, :integer, amount}]} -> {:limit, amount} end)
  end

  defp optional_offset() do
    switch([
      {keyword(:offset), non_neg_integer()},
      {:else, noop()}
    ])
    |> map(fn {[:offset], [{:constant, :integer, amount}]} -> {:offset, amount} end)
  end

  defp optional_having() do
    switch([
      {keyword(:having), having_expressions()},
      {:else, noop()}
    ])
    |> map(fn
      {[:having], [having_expressions]} -> {:having, having_expressions}
      other -> other
    end)
  end

  defp having_expressions(), do: conjunction_expression(having_expression(), "Invalid having expression.")

  defp having_expression() do
    switch([
      {column() |> keyword(:between), allowed_where_range()},
      {column(), pair_both(comparator(), column())},
      {:else, error_message(fail(""), "Invalid having expression.")}
    ])
    |> map(fn
      {[column], [{comparator, value}]} -> create_comparison(column, comparator, value)
      {[column, :between], [{min, max}]} ->
        {:and, {:comparison, column, :>=, min}, {:comparison, column, :<=, max}}
    end)
  end

  defp optional_distinct() do
    keyword(:distinct)
    |> option()
    |> map(fn
      (:distinct) -> {:distinct?, true}
      (nil) -> {:distinct?, false}
    end)
  end

  defp parameter() do
    token(:parameter)
    |> map(&{:parameter, &1.value})
    |> label("expected parameter")
  end

  defp invert_inequality(:<), do: :>=
  defp invert_inequality(:>), do: :<=
  defp invert_inequality(:<=), do: :>
  defp invert_inequality(:>=), do: :<
  defp invert_inequality(:=), do: :=
  defp invert_inequality(:<>), do: :<>

  defp create_comparison({:constant, _, _} = lhs, comparator, rhs), do:
    {:comparison, rhs, invert_inequality(comparator), lhs}
  defp create_comparison(lhs, comparator, rhs), do:
    {:comparison, lhs, comparator, rhs}

  defp optional_sample_users() do
    switch([
      {keyword(:sample_users), pair_both(numeric_constant(:integer), keyword(:%))},
      {:else, noop()}
    ])
    |> map(fn {[:sample_users], [{{:constant, :integer, amount}, :%}]} -> {:sample_rate, amount} end)
  end
end
