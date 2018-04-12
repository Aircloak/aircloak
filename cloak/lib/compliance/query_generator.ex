defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  import StreamData, except: [positive_integer: 0]
  alias Cloak.DataSource.Table
  alias Cloak.Sql.Function

  @data_types [:boolean, :integer, :real, :text, :datetime, :time, :date, :interval]
  @keywords ~w(in is as on or by and from select from left right cast substring)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generates a random AST representing a query into the given tables."
  @spec ast_generator([Table.t()]) :: Stream.t(ast)
  def ast_generator(tables) do
    tables
    |> ast_with_info()
    |> map(fn {ast, _info} -> ast end)
    |> scale(fn size ->
      size |> :math.log() |> trunc() |> max(1)
    end)
  end

  @doc "Generates the SQL query string from the given AST."
  @spec ast_to_sql(ast) :: String.t()
  def ast_to_sql(ast), do: __MODULE__.Format.ast_to_sql(ast)

  # -------------------------------------------------------------------
  # Generators
  # -------------------------------------------------------------------

  defp ast_with_info(tables) do
    bind(from(tables), fn {from_ast, tables} ->
      bind(select(tables), fn {select_ast, info} ->
        bind(order(tables), fn order_clauses ->
          {
            {:query, nil,
             fixed_list(
               [
                 constant(select_ast),
                 constant(from_ast),
                 tables |> where() |> optional(),
                 tables |> group_by() |> optional(),
                 tables |> having() |> optional()
               ] ++ Enum.map(order_clauses, &constant/1) ++ [optional(sample_users())]
             )},
            constant(info)
          }
        end)
      end)
    end)
  end

  defp sample_users(), do: float() |> resize(7) |> map(&{:sample_users, &1, []})

  defp from(tables),
    do: tables |> from_expression() |> map(fn {from_ast, tables} -> {{:from, nil, [from_ast]}, tables} end)

  defp from_expression(tables), do: one_of([from_table(tables), from_subquery(tables), from_join(tables)])

  defp from_subquery(tables) do
    bind(name(), fn name ->
      map(ast_with_info(tables), fn {ast, info} ->
        {as_expression({:subquery, nil, [ast]}, name), [table_from_ast_info(name, info)]}
      end)
    end)
  end

  defp from_join(tables) do
    join_element = join_element(tables)

    tree(join_element, fn child_data ->
      bind(child_data, fn {lhs, lhs_tables} ->
        bind(join_element, fn {rhs, rhs_tables} ->
          tables = lhs_tables ++ rhs_tables
          {{:join, nil, fixed_list([constant(lhs), constant(rhs), on_expression(tables)])}, constant(tables)}
        end)
      end)
    end)
  end

  defp join_element(tables), do: one_of([aliased_table(tables), from_subquery(tables)])

  defp aliased_table(tables) do
    name()
    |> bind(fn alias ->
      tables
      |> from_table()
      |> map(fn {table_ast, [table_info]} ->
        {as_expression(table_ast, alias), [%{table_info | name: alias}]}
      end)
    end)
  end

  defp from_table(tables), do: tables |> member_of() |> map(&{{:table, &1.name, []}, [&1]})

  defp as_expression(object, name), do: {:as, name, [object]}

  defp on_expression(tables), do: tables |> condition() |> map(&{:on, nil, [&1]})

  defp table_from_ast_info(name, ast_info) do
    %{
      name: name,
      columns: Enum.map(ast_info, fn {type, name} -> %{name: name, type: type} end)
    }
  end

  defp where(tables), do: tables |> condition() |> map(&{:where, nil, [&1]})

  defp group_by(tables),
    do: tables |> unaliased_expression(:any) |> list_of() |> nonempty() |> map(&{:group_by, nil, &1})

  defp having(tables), do: tables |> simple_condition() |> map(&{:having, nil, [&1]})

  defp order(tables) do
    {order_by(tables), limit(), offset()}
    |> bind(fn {order_by, limit, offset} ->
      member_of([[], [order_by], [order_by, limit], [order_by, limit, offset]])
    end)
  end

  defp limit(), do: positive_integer() |> map(&{:limit, &1, []})

  defp offset(), do: positive_integer() |> map(&{:offset, &1, []})

  defp order_by(tables), do: tables |> order_item() |> list_of(min_length: 1) |> map(&{:order_by, nil, &1})

  defp order_item(tables) do
    tables
    |> unaliased_expression(:any, _aggregates_allowed? = true)
    |> bind(fn expression ->
      {:order_spec, nil, fixed_list([constant(expression), optional(order_direction()), optional(nulls_directive())])}
    end)
  end

  defp order_direction(), do: [:asc, :desc] |> member_of() |> map(&{:order_direction, &1, []})

  defp nulls_directive(), do: [:first, :last] |> member_of() |> map(&{:nulls, &1, []})

  defp simple_condition(tables) do
    [
      between(tables, _aggregates_allowed? = true),
      comparison(tables, _aggregates_allowed? = true),
      implicit_condition(tables, _aggregates_allowed? = true)
    ]
    |> one_of()
    |> tree(&logical_condition/1)
  end

  defp condition(tables) do
    [between(tables), like(tables), in_expression(tables), comparison(tables), implicit_condition(tables)]
    |> one_of()
    |> tree(&logical_condition/1)
  end

  defp implicit_condition(tables, aggregates_allowed? \\ false),
    do: unaliased_expression(tables, :boolean, aggregates_allowed?)

  defp logical_condition(child_data) do
    one_of([
      {:and, nil, fixed_list([child_data, child_data])},
      {:or, nil, fixed_list([child_data, child_data])},
      {:not, nil, fixed_list([child_data])}
    ])
  end

  defp in_expression(tables) do
    tables
    |> unaliased_expression_with_info(:any)
    |> bind(fn {column, {type, _}} ->
      tuple({
        member_of([:in, :not_in]),
        constant(nil),
        fixed_list([constant(column), in_set(type)])
      })
    end)
  end

  defp in_set(type), do: type |> value() |> list_of() |> nonempty() |> map(&{:in_set, nil, &1})

  defp like(tables) do
    tuple({
      member_of([:like, :ilike, :not_like, :not_ilike]),
      constant(nil),
      fixed_list([unaliased_expression(tables, :text), value(:like_pattern)])
    })
  end

  defp comparison(tables, aggregates_allowed? \\ false) do
    tables
    |> unaliased_expression_with_info(:any, aggregates_allowed?)
    |> bind(fn {column, {type, _}} ->
      tuple({
        member_of([:=, :<>, :<, :>]),
        constant(nil),
        fixed_list([constant(column), unaliased_expression(tables, type, aggregates_allowed?)])
      })
    end)
  end

  defp between(tables, aggregates_allowed? \\ false) do
    tables
    |> unaliased_expression_with_info(:any, aggregates_allowed?)
    |> bind(fn {column, {type, _}} ->
      tuple({
        constant(:between),
        constant(nil),
        fixed_list([constant(column), value(type), value(type)])
      })
    end)
  end

  defp value_with_info(type) do
    type
    |> value()
    |> map(fn
      expression = {:cast, type, _} -> {expression, {type, ""}}
      expression = {type, _, _} -> {expression, {type, ""}}
    end)
  end

  defp value({:constant, type}), do: value(type)
  defp value({:many1, type}), do: value(type)
  defp value({:optional, type}), do: value(type)
  defp value({:or, types}), do: types |> member_of() |> bind(&value/1)
  defp value(:any), do: @data_types |> member_of() |> bind(&value/1)
  defp value(:boolean), do: map(boolean(), &{:boolean, &1, []})
  defp value(:integer), do: map(integer(), &{:integer, &1, []})
  defp value(:real), do: map(float(), &{:real, &1, []})

  defp value(:text), do: escaped_string() |> map(&{:text, &1, []})

  defp value(:date), do: naive_date_time() |> map(&NaiveDateTime.to_date/1) |> map(&build_cast(&1, :date))
  defp value(:time), do: naive_date_time() |> map(&NaiveDateTime.to_time/1) |> map(&build_cast(&1, :time))
  defp value(:datetime), do: naive_date_time() |> map(&build_cast(&1, :datetime))
  defp value(:interval), do: integer() |> map(&Timex.Duration.from_seconds/1) |> map(&{:interval, &1, []})

  defp value(:like_pattern) do
    {escaped_string(), optional(like_escape())}
    |> map(fn {string, escape} -> {:like_pattern, string, [escape]} end)
  end

  defp build_cast(value, type), do: {:cast, type, [{:text, to_string(value), []}]}

  defp like_escape(), do: map(escaped_string(length: 1), &{:like_escape, &1, []})

  defp naive_date_time() do
    fixed_map(%{
      year: integer(1950..2050),
      month: integer(1..12),
      day: integer(1..28),
      hour: integer(0..23),
      minute: integer(0..59),
      second: integer(0..59)
    })
    |> map(&struct(NaiveDateTime, &1))
  end

  defp select(tables) do
    tables
    |> select_list()
    |> bind(fn items ->
      {select_list, info} = Enum.unzip(items)

      member_of([
        {{:select, nil, [{:select_list, nil, select_list}]}, info},
        {{:select, nil, [{:distinct, nil, [{:select_list, nil, select_list}]}]}, info}
      ])
    end)
  end

  defp select_list(tables), do: tables |> expression_with_info() |> list_of() |> nonempty()

  defp expression_with_info(tables, aggregates_allowed? \\ true) do
    one_of([
      aliased_expression_with_info(tables, aggregates_allowed?),
      unaliased_expression_with_info(tables, :any, aggregates_allowed?)
    ])
  end

  defp aliased_expression_with_info(tables, aggregates_allowed?) do
    tables
    |> unaliased_expression_with_info(:any, aggregates_allowed?)
    |> bind(fn {expression, {type, _name}} ->
      map(name(), fn name -> {as_expression(expression, name), {type, name}} end)
    end)
  end

  defp unaliased_expression(tables, type, aggregates_allowed? \\ false),
    do: tables |> unaliased_expression_with_info(type, aggregates_allowed?) |> map(&strip_info/1)

  defp unaliased_expression_with_info(tables, type, aggregates_allowed? \\ false) do
    star_frequency = if(aggregates_allowed?, do: 1, else: 0)
    variable_frequency = if(constant?(type), do: 0, else: 1)
    regular_frequency = 5

    sized(fn size ->
      frequency([
        {regular_frequency * variable_frequency, column_with_info(tables, type)},
        {regular_frequency, value_with_info(type)},
        {regular_frequency * star_frequency * variable_frequency, count_star(type)},
        {regular_frequency * size, tables |> function_with_info(type, aggregates_allowed?) |> resize(div(size, 2))},
        {size, tables |> special_function_with_info(type, aggregates_allowed?) |> resize(div(size, 2))}
      ])
      |> filter(& &1, _max_tries = 100)
    end)
  end

  @functions ~w(
    abs btrim ceil concat date_trunc day extract_words floor hash hex hour left length lower ltrim minute month quarter
    right round rtrim second sqrt trunc upper weekday year count avg min max stddev count_noise avg_noise stddev_noise
    + - * / ^
  )
  defp function_with_info(tables, type, aggregates_allowed?) do
    @functions
    |> Enum.filter(fn function -> aggregates_allowed? or not Function.aggregator?(function) end)
    |> Enum.flat_map(fn function ->
      function
      |> Function.type_specs()
      |> Enum.map(fn {argument_types, return_type} ->
        {function, argument_types, return_type}
      end)
    end)
    |> Enum.filter(fn {_, _, return_type} -> match_type?(type, return_type) end)
    |> case do
      [] ->
        constant(nil)

      candidates ->
        candidates
        |> member_of()
        |> bind(fn {function, argument_types, return_type} ->
          function_arguments(tables, function, argument_types, aggregates_allowed?, constant?(type))
          |> map(fn arguments -> {{:function, function, arguments}, {return_type, function}} end)
        end)
    end
  end

  defp special_function_with_info(tables, type, aggregates_allowed?) do
    substring_frequency = if(match_type?(type, :text), do: 1, else: 0)

    frequency([
      {1, cast(tables, type, aggregates_allowed?)},
      {substring_frequency, substring(tables, aggregates_allowed?)}
    ])
  end

  defp cast(tables, type, aggregates_allowed?) do
    @data_types
    |> Enum.filter(&match_type?(type, &1))
    |> member_of()
    |> bind(fn type ->
      function = {:cast, type}

      function
      |> Function.type_specs()
      |> member_of()
      |> bind(fn {argument_types, return_type} ->
        function_arguments(tables, function, argument_types, aggregates_allowed?, constant?(type))
        |> map(fn arguments -> {{:cast, type, arguments}, {return_type, ""}} end)
      end)
    end)
  end

  defp substring(tables, aggregates_allowed?) do
    {do_function_arguments(tables, [:text], aggregates_allowed?), list_of(positive_integer_value(), length: 2)}
    |> bind(fn {[text], [from, for]} ->
      [
        {:substring, nil, [text, {:keyword_arg, :from, [from]}]},
        {:substring, nil, [text, {:keyword_arg, :for, [for]}]},
        {:substring, nil, [text, {:keyword_arg, :from, [from]}, {:keyword_arg, :for, [for]}]}
      ]
      |> member_of()
    end)
    |> map(&{&1, {:text, "substring"}})
  end

  defp positive_integer_value(), do: map(positive_integer(), &{:integer, &1, []})

  defp function_arguments(tables, function, argument_types, aggregates_allowed?, constant?) do
    distinct_frequency = if(Function.aggregator?(function), do: 1, else: 0)
    argument_types = if(constant?, do: Enum.map(argument_types, &{:constant, &1}), else: argument_types)

    frequency([
      {1, do_function_arguments(tables, argument_types, aggregates_allowed?)},
      {distinct_frequency, distinct_argument(tables, argument_types, aggregates_allowed?)}
    ])
  end

  defp distinct_argument(tables, argument_types, aggregates_allowed?) do
    do_function_arguments(tables, argument_types, aggregates_allowed?)
    |> map(&[{:distinct, nil, &1}])
  end

  defp do_function_arguments(tables, argument_types, aggregates_allowed?) do
    argument_types
    |> Enum.map(&unaliased_expression(tables, &1, aggregates_allowed?))
    |> fixed_list()
  end

  defp count_star(expected_type) when expected_type in [:any, :integer] do
    ~w(count count_noise)
    |> member_of()
    |> map(&{{:function, &1, [{:star, nil, []}]}, {:integer, &1}})
  end

  defp count_star(_), do: nil

  defp column_with_info(tables, type) do
    for table <- tables,
        column <- table.columns,
        column.name != "",
        match_type?(type, column.type) do
      {column.name, table.name, column.type}
    end
    |> case do
      [] -> constant(nil)
      candidates -> candidates |> member_of() |> bind(&build_column_reference/1)
    end
  end

  defp build_column_reference({column, table, type}) do
    [
      {:column, nil, fixed_list([identifier(column)])},
      {:column, nil, fixed_list([identifier(table), identifier(column)])}
    ]
    |> one_of()
    |> map(fn reference -> {reference, {type, column}} end)
  end

  defp identifier(text) do
    simple_identifier = ~r/^[a-zA-Z_]*$/

    if Regex.match?(simple_identifier, text) and not (text in @keywords) do
      constant({:unquoted, text, []})
    else
      constant({:quoted, text, []})
    end
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp optional(data), do: one_of([data, constant(empty())])

  defp empty(), do: {:empty, nil, []}

  defp name(), do: string(?a..?z, min_length: 1) |> filter(&(not (&1 in @keywords)))

  defp escaped_string(opts \\ []) do
    frequency([
      {10, string(:ascii, opts)},
      {1, string(:printable, opts)}
    ])
    |> map(&String.replace(&1, "'", "''"))
  end

  defp positive_integer(), do: map(integer(), &(abs(&1) + 1))

  defp match_type?(:any, _), do: true
  defp match_type?({:optional, type}, actual), do: match_type?(type, actual)
  defp match_type?({:constant, type}, actual), do: match_type?(type, actual)
  defp match_type?({:many1, type}, actual), do: match_type?(type, actual)
  defp match_type?({:or, types}, actual), do: Enum.any?(types, &match_type?(&1, actual))
  defp match_type?(type, type), do: true
  defp match_type?(type, _) when type in @data_types, do: false

  defp constant?({:constant, _}), do: true
  defp constant?({:or, types}), do: Enum.all?(types, &constant?/1)
  defp constant?({:many1, type}), do: constant?(type)
  defp constant?({:optional, type}), do: constant?(type)
  defp constant?(_), do: false

  defp strip_info({item, _info}), do: item
end
