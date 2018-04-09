defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  import StreamData
  alias Cloak.DataSource.Table

  @data_types [:boolean, :integer, :real, :text, :datetime, :time, :date, :interval]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generates a random AST representing a query into the given tables."
  @spec ast_generator([Table.t()]) :: Stream.t(ast)
  def ast_generator(tables),
    do:
      tables
      |> ast_with_info()
      |> map(fn {ast, _info} -> ast end)
      |> scale(fn size ->
        size |> :math.log() |> trunc() |> max(1)
      end)

  @doc "Generates the SQL query string from the given AST."
  @spec ast_to_sql(ast) :: String.t()
  def ast_to_sql(ast), do: __MODULE__.Format.ast_to_sql(ast)

  # -------------------------------------------------------------------
  # Generators
  # -------------------------------------------------------------------

  defp ast_with_info(tables),
    do:
      bind(from(tables), fn {from_ast, tables} ->
        bind(select(tables), fn {select_ast, info} ->
          {
            {:query, nil,
             fixed_list([
               constant(select_ast),
               constant(from_ast),
               tables |> where() |> optional(),
               tables |> group_by() |> optional(),
               tables |> having() |> optional(),
               sample_users() |> optional()
             ])},
            constant(info)
          }
        end)
      end)

  defp sample_users(), do: float() |> resize(7) |> map(&{:sample_users, &1, []})

  defp from(tables),
    do: tables |> from_expression() |> map(fn {from_ast, tables} -> {{:from, nil, [from_ast]}, tables} end)

  defp from_expression(tables), do: one_of([from_table(tables), from_subquery(tables), from_join(tables)])

  defp from_subquery(tables),
    do:
      bind(name(), fn name ->
        map(ast_with_info(tables), fn {ast, info} ->
          {as_expression({:subquery, nil, [ast]}, name), [table_from_ast_info(name, info)]}
        end)
      end)

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

  defp aliased_table(tables),
    do:
      name()
      |> bind(fn alias ->
        tables
        |> from_table()
        |> map(fn {table_ast, [table_info]} ->
          {as_expression(table_ast, alias), [%{table_info | name: alias}]}
        end)
      end)

  defp from_table(tables), do: tables |> member_of() |> map(&{{:table, &1.name, []}, [&1]})

  defp as_expression(object, name), do: {:as, name, [object]}

  defp on_expression(tables), do: tables |> condition() |> map(&{:on, nil, [&1]})

  defp table_from_ast_info(name, ast_info),
    do: %{
      name: name,
      columns: Enum.map(ast_info, fn {type, name} -> %{name: name, type: type} end)
    }

  defp where(tables), do: tables |> condition() |> map(&{:where, nil, [&1]})

  defp group_by(tables),
    do: tables |> unaliased_expression(:any) |> list_of() |> nonempty() |> map(&{:group_by, nil, &1})

  defp having(tables), do: tables |> simple_condition() |> map(&{:having, nil, [&1]})

  defp simple_condition(tables),
    do:
      [between(tables, _aggregates_allowed? = true), comparison(tables, _aggregates_allowed? = true)]
      |> one_of()
      |> tree(&logical_condition/1)

  defp condition(tables),
    do:
      [between(tables), like(tables), in_expression(tables), comparison(tables)]
      |> one_of()
      |> tree(&logical_condition/1)

  defp logical_condition(child_data), do: {member_of([:and, :or]), nil, fixed_list([child_data, child_data])}

  defp in_expression(tables),
    do:
      tables
      |> unaliased_expression_with_info(:any)
      |> bind(fn {column, {type, _}} ->
        tuple({
          member_of([:in, :not_in]),
          constant(nil),
          fixed_list([constant(column), in_set(type)])
        })
      end)

  defp in_set(type), do: type |> value() |> list_of() |> nonempty() |> map(&{:in_set, nil, &1})

  defp like(tables),
    do:
      tuple({
        member_of([:like, :ilike, :not_like, :not_ilike]),
        constant(nil),
        fixed_list([unaliased_expression(tables, :text), value(:like_pattern)])
      })

  defp comparison(tables, aggregates_allowed? \\ false),
    do:
      tables
      |> unaliased_expression_with_info(:any, aggregates_allowed?)
      |> bind(fn {column, {type, _}} ->
        tuple({
          member_of([:=, :<>, :<, :>]),
          constant(nil),
          fixed_list([constant(column), unaliased_expression(tables, type, aggregates_allowed?)])
        })
      end)

  defp between(tables, aggregates_allowed? \\ false),
    do:
      tables
      |> unaliased_expression_with_info(:any, aggregates_allowed?)
      |> bind(fn {column, {type, _}} ->
        tuple({
          constant(:between),
          constant(nil),
          fixed_list([constant(column), value(type), value(type)])
        })
      end)

  defp value_with_info(type) do
    type
    |> value()
    |> map(fn
      expression = {:function, "cast", [_, {:type, type, _}]} -> {expression, {type, ""}}
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

  defp value(:text), do: string_without_quote() |> filter(&(not String.contains?(&1, "'"))) |> map(&{:text, &1, []})

  defp value(:date), do: naive_date_time() |> map(&NaiveDateTime.to_date/1) |> map(&build_cast(&1, :date))
  defp value(:time), do: naive_date_time() |> map(&NaiveDateTime.to_time/1) |> map(&build_cast(&1, :time))
  defp value(:datetime), do: naive_date_time() |> map(&build_cast(&1, :datetime))
  defp value(:interval), do: integer() |> map(&Timex.Duration.from_seconds/1) |> map(&{:interval, &1, []})

  defp value(:like_pattern),
    do:
      like_escape()
      |> bind(fn escape ->
        map(string_without_quote(), &{:like_pattern, &1, [escape]})
      end)

  defp build_cast(value, type) do
    {:function, "cast", [{:text, to_string(value), []}, {:type, type, []}]}
  end

  defp like_escape(),
    do:
      one_of([
        constant(empty()),
        map(string_without_quote(length: 1), &{:like_escape, [&1], []})
      ])

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

  defp select(tables),
    do:
      tables
      |> select_list()
      |> map(fn items ->
        {select_list, info} = Enum.unzip(items)
        {{:select, nil, select_list}, info}
      end)

  defp select_list(tables),
    do:
      tables
      |> expression_with_info()
      |> list_of()
      |> nonempty()

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

    sized(fn size ->
      frequency([
        {1, column_with_info(tables, type)},
        {1, value_with_info(type)},
        {star_frequency, count_star(type)},
        {size, resize(function_with_info(tables, type, aggregates_allowed?), div(size, 2))}
      ])
      |> filter(& &1)
    end)
  end

  @functions ~w(
    abs btrim ceil concat date_trunc day extract_words floor hash hex hour left length lower ltrim minute month quarter
    right round rtrim second sqrt trunc upper weekday year count avg min max stddev count_noise avg_noise stddev_noise
    + - * / ^
  )
  defp function_with_info(tables, type, aggregates_allowed?) do
    @functions
    |> Enum.filter(fn function -> aggregates_allowed? or not Cloak.Sql.Function.aggregator?(function) end)
    |> Enum.flat_map(fn function ->
      function
      |> Cloak.Sql.Function.type_specs()
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
          arguments = Enum.map(argument_types, &unaliased_expression(tables, &1, aggregates_allowed?))
          {{:function, constant(function), fixed_list(arguments)}, {constant(return_type), constant(function)}}
        end)
    end
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
      {{:column, {column.name, table.name}, []}, {column.type, column.name}}
    end
    |> case do
      [] -> constant(nil)
      candidates -> member_of(candidates)
    end
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp optional(data), do: one_of([data, constant(empty())])

  defp empty(), do: {:empty, nil, []}

  @keywords ~w(in is as on or by from select)
  defp name(), do: string(?a..?z, min_length: 1) |> filter(&(not (&1 in @keywords)))

  defp string_without_quote(opts \\ []), do: string(:ascii, opts) |> filter(&(not String.contains?(&1, "'")))

  defp match_type?(:any, _), do: true
  defp match_type?({:optional, type}, actual), do: match_type?(type, actual)
  defp match_type?({:constant, type}, actual), do: match_type?(type, actual)
  defp match_type?({:many1, type}, actual), do: match_type?(type, actual)
  defp match_type?({:or, types}, actual), do: Enum.any?(types, &match_type?(&1, actual))
  defp match_type?(type, type), do: true
  defp match_type?(type, _) when type in @data_types, do: false

  defp strip_info({item, _info}), do: item
end
