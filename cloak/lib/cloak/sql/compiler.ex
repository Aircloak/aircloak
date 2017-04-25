defmodule Cloak.Sql.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.Sql.{Expression, Comparison, FixAlign, Function, Parser, Query, TypeChecker, Range}
  alias Cloak.Query.DataDecoder

  defmodule CompilationError do
    @moduledoc false
    defexception message: "Error during compiling query"
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(DataSource.t, Parser.parsed_query, [Query.parameter] | nil, Query.view_map) ::
    {:ok, Query.t} | {:error, String.t}
  def compile(data_source, parsed_query, parameters, views) do
    try do
      %Query{
        data_source: data_source,
        parameters: parameters,
        views: views
      }
      |> Map.merge(parsed_query)
      |> compile_prepped_query()
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end

  @doc "Validates a user-defined view."
  @spec validate_view(DataSource.t, Parser.parsed_query, Query.view_map) :: :ok | {:error, String.t}
  def validate_view(data_source, parsed_query, views) do
    try do
      with {:ok, query} <- compile(data_source, Map.put(parsed_query, :subquery?, true), [], views), do:
        {:ok, query |> validate_uid("the view") |> validate_offset("The view")}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end

  @doc "Creates the query which describes a SELECT statement from a single table."
  @spec make_select_query(DataSource.t, String.t, [Expression.t]) :: Query.t
  def make_select_query(data_source, table_name, select_expressions) do
    column_titles = for expression <- select_expressions, do: expression.alias || expression.name
    calculate_db_columns(%Query{
      command: :select,
      subquery?: true,
      columns: select_expressions,
      column_titles: column_titles,
      from: table_name,
      data_source: data_source,
      selected_tables: [DataSource.table(data_source, table_name)]
    })
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_prepped_query(%Query{command: :show, show: :tables} = query), do:
    {:ok, %Query{query |
      column_titles: ["name"],
      columns: [%Expression{table: :unknown, constant?: true, name: "name", type: :text}]
    }}
  defp compile_prepped_query(%Query{command: :show, show: :columns} = query), do:
    {:ok, %Query{compile_from(query) |
      column_titles: ["name", "type"],
      columns: Enum.map(["name", "type"], &%Expression{table: :unknown, constant?: true, name: &1, type: :text})
    }}
  defp compile_prepped_query(%Query{command: :select} = query), do:
    {:ok,
      query
      |> compile_from()
      |> compile_parameter_types()
      |> compile_columns()
      |> reject_null_user_ids()
      |> verify_columns()
      |> precompile_functions()
      |> censor_selected_uids()
      |> compile_order_by()
      |> verify_joins()
      |> cast_where_clauses()
      |> verify_where_clauses()
      |> align_ranges(Lens.key(:where), :where)
      |> align_join_ranges()
      |> add_subquery_ranges()
      |> verify_having()
      |> set_emulation_flag()
      |> partition_where_clauses()
      |> calculate_db_columns()
      |> verify_limit()
      |> verify_offset()
      |> TypeChecker.validate_allowed_usage_of_math_and_functions()
      |> optimize_columns_from_projected_tables()
      |> parse_row_splitters()
      |> partition_selected_columns()
    }


  # -------------------------------------------------------------------
  # Views
  # -------------------------------------------------------------------

  defp resolve_views(query) do
    compiled = do_resolve_views(query.from, query)
    %Query{query | from: compiled}
  end

  defp do_resolve_views({:join, join}, query) do
    {:join, %{join |
      lhs: do_resolve_views(join.lhs, query),
      rhs: do_resolve_views(join.rhs, query)
    }}
  end
  defp do_resolve_views({_, name} = table_or_view, query) when is_binary(name) do
    case Map.fetch(query.views, name) do
      {:ok, view_sql} -> view_to_subquery(name, view_sql, query)
      :error -> table_or_view
    end
  end
  defp do_resolve_views(other, _query), do:
    other

  def view_to_subquery(view_name, view_sql, query) do
    if Enum.any?(
      query.data_source.tables,
      fn({_id, table}) -> insensitive_equal?(table.name, view_name) end
    ) do
      raise CompilationError,
        message: "There is both a table, and a view named `#{view_name}`. Rename the view to resolve the conflict."
    end

    case Cloak.Sql.Parser.parse(view_sql) do
      {:ok, parsed_view} -> {:subquery, %{type: :parsed, ast: parsed_view, alias: view_name}}
      {:error, error} -> raise CompilationError, message: "Error in the view `#{view_name}`: #{error}"
    end
  end


  # -------------------------------------------------------------------
  # Projected tables
  # -------------------------------------------------------------------

  defp resolve_projected_tables(%Query{projected?: true} = query), do: query
  defp resolve_projected_tables(query), do:
    Lens.map(Query.Lenses.leaf_tables(), query, &resolve_projected_table(&1, query))

  defp resolve_projected_table(table_name, query) do
    case DataSource.table(query.data_source, table_name) do
      %{projection: nil} -> table_name
      projected_table -> projected_table_ast(projected_table, :all, query)
    end
  end

  defp projected_table_ast(%{projection: nil} = table, _column_to_select, _query), do:
    {:quoted, table.name}
  defp projected_table_ast(table, columns_to_select, query) do
    joined_table = DataSource.table(query.data_source, table.projection.table)
    {:subquery, %{
      type: :parsed,
      alias: table.name,
      ast: %{
        command: :select,
        projected?: true,
        columns:
          [column_ast(joined_table.name, joined_table.user_id) |
            table.columns
            |> Enum.map(fn({column_name, _type}) -> column_name end)
            |> Enum.reject(&(&1 == table.user_id))
            |> Enum.filter(&(columns_to_select == :all || Enum.member?(columns_to_select, &1)))
            |> Enum.map(&column_ast(table.name, &1))
          ],
        from:
          {:join, %{
            type: :inner_join,
            lhs: {:quoted, table.name},
            rhs: projected_table_ast(joined_table, [table.projection.primary_key], query),
            conditions: [{:comparison,
              column_ast(table.name, table.projection.foreign_key),
              :=,
              column_ast(joined_table.name, table.projection.primary_key)
            }]
          }}
      }
    }}
  end

  defp column_ast(table_name, column_name), do:
    {:identifier, {:quoted, table_name}, {:quoted, column_name}}

  defp optimize_columns_from_projected_tables(%Query{projected?: false} = query), do:
    # We're reducing the amount of selected columns from projected subqueries to only
    # those columns which we in fact need in the outer query (`query`).
    #
    # Notice that this has to be done after all verifications have been performed. The reason is that we're
    # conflating the list of selected columns and the list of available columns in the field `columns`.
    # Therefore, we need to perform all checks with all projected table columns selected, and only then can
    # we optimize the list of selected columns from the projected subquery.
    #
    # These two fields should likely be separated, and then we could invoke this function earlier. However,
    # even then, this function can only be invoked after `db_columns` have been calculated, because that is
    # the field we use to decide which columns from projected tables do we in fact need.
    Lens.map(Query.Lenses.direct_projected_subqueries(), query,
        &%{&1 | ast: optimized_projected_subquery_ast(&1.ast, required_column_names(query, &1))})
  defp optimize_columns_from_projected_tables(%Query{projected?: true} = query), do:
    # If this query is projected, then the list was already optimized when the ast for this query
    # has been initially generated, so no need to do anything.
    query

  defp required_column_names(query, projected_subquery), do:
    [
      # append uid column
      DataSource.table(projected_subquery.ast.data_source, projected_subquery.alias).user_id |
      # all db columns of the outer query which are from this projected table
      query |> Query.required_columns_from_table(projected_subquery.alias) |> Enum.map(& &1.name)
    ]

  defp optimized_projected_subquery_ast(ast, required_column_names) do
    columns = Enum.filter(ast.columns, & &1.name in required_column_names)
    titles = Enum.filter(ast.column_titles, & &1 in required_column_names)
    %Query{ast |
      next_row_index: 0, db_columns: [],
      columns: columns, property: columns,
      column_titles: titles
    }
    |> calculate_db_columns()
  end


  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp compile_subqueries(query) do
    {info, compiled} = Lens.get_and_map(Query.Lenses.direct_subqueries(), query, fn(subquery) ->
      ast = compiled_subquery(subquery.ast, subquery.alias, query)
      {ast.info, %{subquery | ast: ast}}
    end)

    Query.add_info(compiled, Enum.concat(info))
  end

  defp compiled_subquery(parsed_subquery, alias, parent_query) do
    case compile(
      parent_query.data_source,
      Map.put(parsed_subquery, :subquery?, :true),
      parent_query.parameters,
      parent_query.views
    ) do
      {:ok, compiled_query} ->
        compiled_query
        |> validate_uid("subquery `#{alias}`")
        |> validate_offset("Subquery `#{alias}`")
        |> align_limit()
        |> align_offset()
        |> align_ranges(Lens.key(:having), :having)
        |> carry_ranges()
      {:error, error} -> raise CompilationError, message: error
    end
  end

  defp carry_ranges(query) do
    query = %{query | ranges: Enum.flat_map(query.ranges, &carrying_ranges(query, &1))}
    range_columns = Enum.map(query.ranges, &(&1.column))

    if query.emulated? do
      %{
        query |
        columns: query.columns ++ range_columns,
        column_titles: query.column_titles ++ Enum.map(range_columns, & &1.alias),
        aggregators: query.aggregators ++ Enum.filter(range_columns, & &1.aggregate?),
      }
    else
      %{query | db_columns: query.db_columns ++ range_columns}
    end
  end

  defp carrying_ranges(%{implicit_count?: true}, range), do: [%{range | column: alias_column(range.column)}]
  defp carrying_ranges(_query, range = %{type: type, column: column}) do
    case type do
      :having -> [%{range | type: :where, column: alias_column(column)}]
      :nested_min -> [%{range | column: min_column(column)}]
      :nested_max -> [%{range | column: max_column(column)}]
      :where -> [
        %{range | type: :nested_min, column: min_column(column)},
        %{range | type: :nested_max, column: max_column(column)},
      ]
    end
  end

  defp min_column(column), do: Expression.function("min", [column], column.type, true) |> alias_column()

  defp max_column(column), do: Expression.function("max", [column], column.type, true) |> alias_column()

  defp alias_column(column), do: %{column | alias: new_carry_alias()}

  defp new_carry_alias(), do: "carry_#{System.unique_integer([:positive])}"

  @minimum_subquery_limit 10
  defp align_limit(query = %{limit: nil}), do: query
  defp align_limit(query = %{limit: limit}) do
    aligned = limit |> FixAlign.align() |> round() |> max(@minimum_subquery_limit)
    if aligned != limit do
      %{query | limit: aligned}
      |> Query.add_info("Limit adjusted from #{limit} to #{aligned}")
    else
      query
    end
  end

  defp align_offset(query = %{offset: 0}), do: query
  defp align_offset(query = %{limit: limit, offset: offset}) do
    aligned = round(offset / limit) * limit
    if aligned != offset do
      %{query | offset: aligned}
      |> Query.add_info("Offset adjusted from #{offset} to #{aligned}")
    else
      query
    end
  end

  defp validate_uid(subquery, display) do
    case Enum.find(subquery.columns, &(&1.user_id?)) do
      nil ->
        possible_uid_columns =
          all_id_columns_from_tables(subquery)
          |> Enum.map(&Expression.display_name/1)
          |> case do
            [column] -> "the column #{column}"
            columns -> "one of the columns #{Enum.join(columns, ", ")}"
          end

        raise CompilationError, message:
          "Missing a user id column in the select list of #{display}. " <>
          "To fix this error, add #{possible_uid_columns} to the subquery select list."
      _ ->
        subquery
    end
  end

  defp validate_offset(%{offset: offset, limit: limit}, display) when is_nil(limit) and offset > 0, do:
      raise CompilationError, message: "#{display} has an OFFSET clause without a LIMIT clause."
  defp validate_offset(subquery, _), do: subquery


  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp compile_from(query), do:
    query
    |> resolve_views()
    |> normalize_from()
    |> resolve_projected_tables()
    |> compile_subqueries()
    |> resolve_selected_tables()

  defp normalize_from(%Query{} = query), do:
    %Query{query | from: normalize_from(query.from, query.data_source)}

  defp normalize_from({:join, join = %{lhs: lhs, rhs: rhs}}, data_source) do
    {:join, %{join | lhs: normalize_from(lhs, data_source), rhs: normalize_from(rhs, data_source)}}
  end
  defp normalize_from(subquery = {:subquery, _}, _data_source), do: subquery
  defp normalize_from(table_identifier = {_, table_name}, data_source) do
    case data_source.tables |> Map.values() |> find_table(table_identifier) do
      nil -> raise CompilationError, message: "Table `#{table_name}` doesn't exist."
      table -> table.name
    end
  end

  defp find_table(tables, {:quoted, name}), do: Enum.find(tables, &name == &1.name)
  defp find_table(tables, {:unquoted, name}), do: Enum.find(tables, &insensitive_equal?(name, &1.name))

  defp resolve_selected_tables(query), do:
    %Query{query | selected_tables: selected_tables(query.from, query.data_source) |> Enum.uniq()}

  defp selected_tables({:join, join}, data_source) do
    selected_tables(join.lhs, data_source) ++ selected_tables(join.rhs, data_source)
  end
  defp selected_tables({:subquery, subquery}, _data_source) do
    # In a subquery we should have the `user_id` already in the list of selected columns.
    user_id_index = Enum.find_index(subquery.ast.columns, &(&1.user_id?))
    user_id_name = Enum.at(subquery.ast.column_titles, user_id_index)
    columns =
        Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
        |> Enum.map(fn ({alias, column}) -> {alias, Function.type(column)} end)
    [%{
      name: subquery.alias,
      columns: columns,
      user_id: user_id_name,
      decoders: [],
      projection: nil
    }]
  end
  defp selected_tables(table_name, data_source) when is_binary(table_name) do
    case DataSource.table(data_source, table_name) do
      nil -> raise CompilationError, message: "Table `#{table_name}` doesn't exist."
      table -> [table]
    end
  end

  defp compile_aliases(%Query{columns: [_|_] = columns} = query) do
    verify_aliases(query)
    column_titles = Enum.map(columns, &column_title(&1, query.selected_tables))
    aliases = for {column, :as, name} <- columns, into: %{}, do: {{:identifier, :unknown, {:unquoted, name}}, column}
    columns = Enum.map(columns, fn ({column, :as, _name}) -> column; (column) -> column end)
    order_by = for {column, direction} <- query.order_by, do: {Map.get(aliases, column, column), direction}
    group_by = for identifier <- query.group_by, do: Map.get(aliases, identifier, identifier)
    where = update_in(query.where, [Query.Lenses.conditions_terminals()], &Map.get(aliases, &1, &1))
    having = update_in(query.having, [Query.Lenses.conditions_terminals()], &Map.get(aliases, &1, &1))
    %Query{query | columns: columns, column_titles: column_titles,
      group_by: group_by, order_by: order_by, where: where, having: having}
  end
  defp compile_aliases(query), do: query

  # Subqueries can produce column-names that are not actually in the table. Without understanding what
  # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
  # the outer column selections
  defp verify_aliases(query) do
    aliases = for {_column, :as, name} <- query.columns, do: name
    all_columns = for {:identifier, _table, {:unquoted, name}} <- all_column_identifiers(query), do: name
    possible_identifiers = aliases ++ all_columns
    referenced_identifiers =
      (for {identifier, _direction} <- query.order_by, do: identifier) ++
      query.group_by ++
      get_in(query.where ++ query.having, [Query.Lenses.conditions_terminals()])
    ambiguous_names = for {:identifier, :unknown, {_, name}} <- referenced_identifiers,
      Enum.count(possible_identifiers, &name == &1) > 1, do: name
    case ambiguous_names do
      [] -> :ok
      [name | _rest] -> raise CompilationError, message: "Usage of `#{name}` is ambiguous."
    end
  end

  defp invalid_individual_columns(%Query{command: :select, group_by: [_|_]} = query), do:
    Enum.filter(query.columns, &individual_column?(&1, query))
  defp invalid_individual_columns(%Query{command: :select} = query) do
    query.columns
    |> Enum.reject(&Expression.constant?/1)
    |> Enum.partition(&aggregated_column?(&1, query))
    |> case  do
      {[_|_] = _aggregates, [_|_] = individual_columns} -> individual_columns
      _ -> []
    end
  end

  defp aggregated_column?(column, query), do:
    Enum.member?(query.group_by, column) or
    (
      column.function? and
      (
        column.aggregate? or
        Enum.any?(column.function_args, &aggregated_column?(&1, query))
      )
    )

  defp individual_column?(column, query), do:
    not Expression.constant?(column) and not aggregated_column?(column, query)

  defp compile_columns(query) do
    query
    |> expand_star_select()
    |> compile_buckets()
    |> compile_aliases()
    |> parse_columns()
  end

  defp compile_buckets(query) do
    {messages, columns} = Lens.get_and_map(Lens.all() |> Query.Lenses.buckets(),
        query.columns, &align_bucket/1)
    Query.add_info(%{query | columns: columns}, Enum.reject(messages, &is_nil/1))
  end

  defp align_bucket(column) do
    if Function.bucket_size(column) <= 0 do
      raise CompilationError, message: "Bucket size #{Function.bucket_size(column)} must be > 0"
    end

    aligned = Function.update_bucket_size(column, &FixAlign.align/1)
    if aligned == column do
      {nil, aligned}
    else
      {"Bucket size adjusted from #{Function.bucket_size(column)} to #{Function.bucket_size(aligned)}", aligned}
    end
  end

  defp expand_star_select(%Query{columns: :*} = query) do
    %Query{query | columns: all_column_identifiers(query)}
  end
  defp expand_star_select(query), do: query

  defp filter_aggregators(columns), do:
    columns
    |> Enum.flat_map(&expand_arguments/1)
    |> Enum.filter(&(match?(%Expression{function?: true, aggregate?: true}, &1)))

  defp verify_columns(query) do
    verify_functions(query)
    verify_aggregated_columns(query)
    verify_group_by_functions(query)
    query
  end

  defp function_argument_error_message({:function, name, _} = function_call) do
    cond do
      Function.cast?(function_call) ->
        [cast_source] = actual_types(function_call)
        cast_target = Function.cast_target(function_call)
        "Cannot cast value of type `#{cast_source}` to type `#{cast_target}`."
      many_overloads?(function_call) ->
        "Arguments of type (#{function_call |> actual_types() |> quoted_list()}) are incorrect"
          <> " for `#{Function.readable_name(name)}`."
      true ->
        "Function `#{Function.readable_name(name)}` requires arguments of type #{expected_types(function_call)}"
          <> ", but got (#{function_call |> actual_types() |> quoted_list()})."
    end
  end

  defp precompile_functions(%Query{} = query), do:
    update_in(query, [Query.Lenses.query_expressions()], &precompile_function/1)
  defp precompile_functions(columns), do:
    Enum.map(columns, &precompile_function/1)

  defp precompile_function(expression = %Expression{function?: true}) do
    case Function.compile_function(expression, &precompile_functions/1) do
      {:error, message} -> raise CompilationError, message: message
      compiled_function -> compiled_function
    end
  end
  defp precompile_function(column), do: column

  defp many_overloads?(function_call) do
    length(Function.argument_types(function_call)) > 4
  end

  defp expected_types(function_call), do:
    Function.argument_types(function_call)
    |> Enum.map(&quoted_list/1)
    |> Enum.map(&"(#{&1})")
    |> Enum.join(" or ")

  defp actual_types(function_call), do:
    Function.arguments(function_call)
    |> Enum.map(fn
      (%Expression{} = expression) -> expression.type
      (:*) -> "unspecified type"
    end)

  defp expand_arguments(column) do
    (column |> Expression.arguments() |> Enum.flat_map(&expand_arguments/1)) ++ [column]
  end

  defp quoted_list(items), do:
    items |> Enum.map(&quoted_item/1) |> Enum.join(", ")

  defp quoted_item({:optional, type}), do: "[`#{type}`]"
  defp quoted_item({:many1, type}), do: "[`#{type}`]+"
  defp quoted_item({:or, types}), do: types |> Enum.map(&quoted_item/1) |> Enum.join(" | ")
  defp quoted_item(item), do: "`#{item}`"

  defp verify_aggregated_columns(query) do
    case invalid_individual_columns(query) do
      [] -> :ok
      [column | _rest] ->
        raise CompilationError, message: "#{aggregated_expression_display(column)} " <>
          "to appear in the `GROUP BY` clause or be used in an aggregate function."
    end
  end

  defp aggregated_expression_display({:function, _function, [arg]}), do:
    "Column #{quoted_item(arg.name)} needs"
  defp aggregated_expression_display({:function, _function, args}), do:
    "Columns (#{args |> Enum.map(&(&1.name)) |> quoted_list()}) need"
  defp aggregated_expression_display(%Expression{function: fun, function_args: args}) when fun != nil do
    [column | _] = for %Expression{constant?: false} = column <- args, do: column
    aggregated_expression_display(column)
  end
  defp aggregated_expression_display(%Expression{table: table, name: name}), do:
    "Column `#{name}` from table `#{table.name}` needs"

  defp verify_group_by_functions(query) do
    query.group_by
    |> Enum.filter(& &1.aggregate?)
    |> case do
      [] -> :ok
      [expression | _] -> raise CompilationError, message:
        "Aggregate function `#{Function.readable_name(expression.function)}` can not be used in the `GROUP BY` clause."
    end
  end

  defp verify_functions(query) do
    query.columns
    |> Enum.filter(&Function.function?/1)
    |> Enum.each(&check_function_validity(&1, query))
  end

  defp check_function_validity(function, query) do
    verify_function_exists(function)
    verify_function_subquery_usage(function, query)
  end

  defp verify_function_exists(function = {_, name, _}) do
    unless Function.exists?(function) do
      raise CompilationError, message: "Unknown function `#{Function.readable_name(name)}`."
    end
  end

  defp verify_function_subquery_usage({:function, name, [argument]}, %Query{subquery?: false})
      when name in ["min", "max", "median"] do
    type = Function.type(argument)
    if Enum.member?([:text, :date, :time, :datetime], type) do
      raise CompilationError, message:
        "Function `#{name}` is allowed over arguments of type `#{type}` only in subqueries."
    end
    :ok
  end
  defp verify_function_subquery_usage(_function, %Query{subquery?: false}), do: :ok
  defp verify_function_subquery_usage({:function, name, _}, %Query{subquery?: true}) do
    if Function.has_attribute?(name, :not_in_subquery) do
      raise CompilationError, message: "Function `#{name}` is not allowed in subqueries."
    end
  end

  defp all_column_identifiers(query) do
    for table <- query.selected_tables, {column_name, _type} <- table.columns do
      {:identifier, table.name, {:unquoted, column_name}}
    end
  end

  defp partition_selected_columns(%Query{group_by: groups = [_|_], columns: selected_columns} = query) do
    having_columns = Enum.flat_map(query.having, fn ({:comparison, column, _operator, target}) -> [column, target] end)
    aggregators = filter_aggregators(selected_columns ++ having_columns)
    %Query{query |
      property: groups |> drop_duplicate_columns_except_row_splitters(),
      aggregators: aggregators |> drop_duplicate_columns_except_row_splitters()
    }
  end
  defp partition_selected_columns(query) do
    case filter_aggregators(query.columns) do
      [] ->
        %Query{query |
          property: query.columns |> drop_duplicate_columns_except_row_splitters(),
          aggregators: [Expression.count_star()],
          implicit_count?: true
        }
      aggregators ->
        %Query{query | property: [], aggregators: aggregators |> drop_duplicate_columns_except_row_splitters()}
    end
  end

  # Drops all duplicate occurrences of columns, with the exception of columns that are, or contain,
  # calls to row splitting functions. This does not affect how many times database columns are loaded
  # from the database, but allows us to deal with the output of the row splitting function instances separately.
  defp drop_duplicate_columns_except_row_splitters(columns), do:
    Enum.uniq_by(columns, &Lens.map(Query.Lenses.splitter_functions(), &1, fn(_) -> Kernel.make_ref() end))

  defp parse_row_splitters(%Query{} = query) do
    {transformed_columns, query} = transform_splitter_columns(query, query.columns)
    validate_row_splitters_conditions(query)
    %Query{query | columns: transformed_columns}
  end

  defp transform_splitter_columns(query, columns) do
    {reversed_transformed_columns, final_query} =
      Enum.reduce(columns, {[], query},
        fn(column, {columns_acc, query_acc}) ->
          {transformed_column, transformed_query} = transform_splitter_column(column, query_acc)
          {[transformed_column | columns_acc], transformed_query}
        end
      )
    {Enum.reverse(reversed_transformed_columns), final_query}
  end

  defp transform_splitter_column(expression = %Expression{function?: true}, query) do
    {transformed_args, query} = transform_splitter_columns(query, Expression.arguments(expression))
    expression = %{expression | function_args: transformed_args}
    if Function.has_attribute?(expression, :row_splitter) do
      # We are making the simplifying assumption that row splitting functions have
      # the value column returned as part of the first column
      {splitter_row_index, query} = add_row_splitter(query, expression)
      db_column = case expression |> Expression.arguments() |> hd() |> Expression.first_column() do
        nil -> raise CompilationError, message:
          "Function `#{Function.readable_name(expression.function)}` requires that the first argument must be a column."
        value -> value
      end
      column_name = "#{Function.readable_name(expression.function)}_return_value"
      return_type = Function.return_type(expression)
      # This, most crucially, preserves the DB row position parameter
      augmented_column = %Expression{db_column | name: column_name, type: return_type, row_index: splitter_row_index}
      # We need to update all other references to this column (group by, propeprty, etc.)
      query = update_in(query, [Query.Lenses.query_expressions()], &if &1 == expression do augmented_column else &1 end)
      {augmented_column, query}
    else
      {expression, query}
    end
  end
  defp transform_splitter_column(other, query), do: {other, query}

  defp add_row_splitter(query, function_spec) do
    case Enum.find(query.row_splitters, &function_spec == &1.function_spec) do
      %{row_index: splitter_row_index} ->
        {splitter_row_index, query}
      nil ->
        {splitter_row_index, query} = Query.next_row_index(query)
        new_splitter = %{function_spec: function_spec, row_index: splitter_row_index}
        {new_splitter.row_index, %Query{query | row_splitters: query.row_splitters ++ [new_splitter]}}
    end
  end

  defp validate_row_splitters_conditions(%Query{emulated_where: conditions}) do
    conditions
    |> get_in([Query.Lenses.conditions_terminals()])
    |> Enum.any?(&Function.has_attribute?(&1, :row_splitter))
    |> if do
      raise CompilationError, message:
        "Row splitter function used in the `WHERE` clause has to be first used identically in the `SELECT` clause."
    end
  end

  defp compile_order_by(%Query{order_by: []} = query), do: query
  defp compile_order_by(%Query{columns: columns, order_by: order_by_spec} = query) do
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) -> Enum.member?(columns, column) end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          index = columns |> Enum.find_index(&(&1 == column))
          {index, direction}
        end
        %Query{query | order_by: order_list}
      [{_column, _direction} | _rest] ->
        raise CompilationError, message: "Non-selected column specified in `ORDER BY` clause."
    end
  end

  defp partition_where_clauses(query) do
    # extract conditions using encoded columns
    {emulated_column_clauses, safe_clauses} = Enum.partition(query.where, &emulated_expression_condition?/1)
    %Query{query | where: safe_clauses, emulated_where: emulated_column_clauses}
  end

  defp verify_joins(%Query{projected?: true} = query), do: query
  defp verify_joins(query) do
    join_conditions_scope_check(query.from)

    # Algorithm for finding improperly joined tables:
    #
    # 1. Create a DCG graph, where all uid columns are vertices.
    # 2. Add an edge for all where clauses shaped as `uid1 = uid2`
    # 3. Find the first pair (uid1, uid2) where there is no path from uid1 to uid2 in the graph.
    # 4. Report an error if something is found in the step 3

    column_key = fn(column) -> {column.name, column.table.name} end

    graph = :digraph.new([:private, :cyclic])
    try do
      # add uid columns as vertices
      uid_columns = Enum.map(query.selected_tables, &%Expression{name: &1.user_id, table: &1})
      Enum.each(uid_columns, &:digraph.add_vertex(graph, column_key.(&1)))

      # add edges for all `uid1 = uid2` filters
      for {:comparison, column1, :=, column2} <- query.where ++ all_join_conditions(query.from),
          column1 != column2,
          column1.user_id?,
          column2.user_id?
      do
        :digraph.add_edge(graph, column_key.(column1), column_key.(column2))
        :digraph.add_edge(graph, column_key.(column2), column_key.(column1))
      end

      # Find first pair (uid1, uid2) which are not connected in the graph.
      uid_columns
      |> Stream.chunk(2, 1)
      |> Stream.filter(
            fn([uid1, uid2]) -> :digraph.get_path(graph, column_key.(uid1), column_key.(uid2)) == false end
          )
      |> Enum.take(1)
      |> case do
            [] ->
              # No such pair -> all tables are properly joined
              query

            [[column1, column2]] ->
              table1 = column1.table.name
              table2 = column2.table.name
              raise CompilationError,
                message:
                  "Missing where comparison for uid columns of tables `#{table1}` and `#{table2}`. " <>
                  "You can fix the error by adding `#{table1}.#{column1.name} = #{table2}.#{column2.name}` " <>
                  "condition to the `WHERE` clause."
          end
    after
      # digraph is powered by ets tables, so we need to make sure they are deleted once we don't need them
      :digraph.delete(graph)
    end
  end

  @spec all_join_conditions(Parser.from_clause) :: [Parser.where_clause]
  defp all_join_conditions({:join, join}) do
    join.conditions ++ all_join_conditions(join.lhs) ++ all_join_conditions(join.rhs)
  end
  defp all_join_conditions(_), do: []

  defp reject_null_user_ids(%Query{subquery?: true} = query), do: query
  defp reject_null_user_ids(query), do:
    %{query | where: [{:not, {:is, id_column(query), :null}} | query.where]}

  defp align_join_ranges(query), do:
    query
    |> Query.Lenses.join_condition_lenses()
    |> Enum.reduce(query, fn(lens, query) -> align_ranges(query, lens, :where) end)

  defp align_ranges(query, lens, range_type) do
    clauses = Lens.get(lens, query)

    verify_ranges(clauses)

    ranges = inequalities_by_column(clauses)
    non_range_clauses = Enum.reject(clauses, &Enum.member?(Map.keys(ranges), Comparison.subject(&1)))

    query = put_in(query, [lens], non_range_clauses)
    Enum.reduce(ranges, query, &add_aligned_range(&1, &2, lens, range_type))
  end

  defp add_aligned_range({column, conditions}, query, lens, range_type) do
    {left, right} =
      conditions
      |> Enum.map(&Comparison.value/1)
      |> Enum.sort(&Cloak.Data.lt_eq/2)
      |> List.to_tuple()
      |> FixAlign.align_interval()

    if implement_range?({left, right}, conditions) do
      update_in(query, [lens], &(conditions ++ &1))
    else
      query
      |> add_clause(lens, {:comparison, column, :<, Expression.constant(column.type, right)})
      |> add_clause(lens, {:comparison, column, :>=, Expression.constant(column.type, left)})
      |> Query.add_info("The range for column #{Expression.display_name(column)} has been adjusted to #{left} <= "
        <> "#{Expression.short_name(column)} < #{right}.")
    end
    |> put_in([Lens.key(:ranges), Lens.front()], Range.new(column, {left, right}, range_type))
  end

  defp implement_range?({left, right}, conditions) do
    [{_, _, left_operator, left_column}, {_, _, right_operator, right_column}] =
      Enum.sort_by(conditions, &Comparison.value/1, &Cloak.Data.lt_eq/2)

    left_operator == :>= && left_column.value == left && right_operator == :< && right_column.value == right
  end

  defp add_clause(query, lens, clause), do: put_in(query, [lens, Lens.front()], clause)

  defp add_subquery_ranges(query) do
    %{query | ranges:
      query
      |> get_in([Query.Lenses.direct_subqueries() |> Lens.key(:ast) |> Lens.key(:ranges) |> Lens.all()])
      |> Enum.map(&(%{&1 | column: %Expression{name: &1.column.alias}}))
      |> Enum.into(query.ranges)
    }
  end

  defp verify_ranges(clauses) do
    clauses
    |> inequalities_by_column()
    |> Enum.reject(fn({_, comparisons}) -> valid_range?(comparisons) end)
    |> case do
      [{column, _} | _] ->
        raise CompilationError, message: "Column #{Expression.display_name(column)} must be limited to a finite range."
      _ -> :ok
    end
  end

  defp valid_range?(comparisons) do
    case Enum.sort_by(comparisons, &Comparison.direction/1, &Kernel.>/2) do
      [cmp1, cmp2] ->
        Comparison.direction(cmp1) != Comparison.direction(cmp2) &&
          Cloak.Data.lt_eq(Comparison.value(cmp1), Comparison.value(cmp2))
      _ -> false
    end
  end

  defp inequalities_by_column(where_clauses) do
    where_clauses
    |> Enum.filter(&Comparison.inequality?/1)
    |> Enum.group_by(&Comparison.subject/1)
    |> Enum.map(&discard_redundant_inequalities/1)
    |> Enum.into(%{})
  end

  defp discard_redundant_inequalities({column, inequalities}) do
    case {bottom, top} = Enum.partition(inequalities, &(Comparison.direction(&1) == :>)) do
      {[], []} -> {column, []}
      {_, []} -> {column, [Enum.max_by(bottom, &Comparison.value/1)]}
      {[], _} -> {column, [Enum.min_by(top, &Comparison.value/1)]}
      {_, _} -> {column, [Enum.max_by(bottom, &Comparison.value/1), Enum.min_by(top, &Comparison.value/1)]}
    end
  end

  defp cast_where_clauses(%Query{where: [_|_] = clauses} = query) do
    %Query{query | where: Enum.map(clauses, &cast_where_clause/1)}
  end
  defp cast_where_clauses(query), do: query

  defp cast_where_clause(clause) do
    column = Comparison.subject(clause)
    do_cast_where_clause(clause, column.type)
  end

  @castable_conditions [:datetime, :time, :date]

  defp do_cast_where_clause({:not, subclause}, type), do:
    {:not, do_cast_where_clause(subclause, type)}
  defp do_cast_where_clause({:comparison, identifier, comparator, rhs}, type) when type in @castable_conditions do
    if Expression.constant?(rhs) do
      {:comparison, identifier, comparator, parse_time(rhs, type)}
    else
      {:comparison, identifier, comparator, rhs}
    end
  end
  defp do_cast_where_clause({:in, column, values}, type) when type in @castable_conditions, do:
    {:in, column, Enum.map(values, &parse_time(&1, type))}
  defp do_cast_where_clause(clause, _), do: clause

  defp parse_time(column = %Expression{constant?: true, value: string}, type) do
    case do_parse_time(column, type) do
      {:ok, result} -> Expression.constant(type, result)
      _ -> raise CompilationError, message: "Cannot cast `#{string}` to #{type}."
    end
  end

  defp do_parse_time(%Expression{type: :text, value: string}, :date), do:
    Cloak.Time.parse_date(string)
  defp do_parse_time(%Expression{type: :text, value: string}, :time), do:
    Cloak.Time.parse_time(string)
  defp do_parse_time(%Expression{type: :text, value: string}, :datetime), do:
    Cloak.Time.parse_datetime(string)
  defp do_parse_time(_, _), do: {:error, :invalid_cast}

  defp map_terminal_elements(query, mapper_fun), do:
    Lens.map(Query.Lenses.terminals(), query, mapper_fun)

  defp parse_columns(query) do
    columns_by_name =
      for table <- query.selected_tables, {column, type} <- table.columns do
        %Expression{table: table, name: column, type: type, user_id?: table.user_id == column}
      end
      |> Enum.group_by(&(&1.name))
    query = map_terminal_elements(query, &normalize_table_name(&1, query.selected_tables))
    map_terminal_elements(query, &identifier_to_column(&1, columns_by_name, query))
  end

  defp normalize_table_name({:identifier, table_identifier = {_, name}, column}, selected_tables) do
    case find_table(selected_tables, table_identifier) do
      nil -> {:identifier, name, column}
      table -> {:identifier, table.name, column}
    end
  end
  defp normalize_table_name(x, _), do: x

  defp identifier_to_column({:identifier, :unknown, identifier = {_, column_name}}, columns_by_name, _query) do
    case get_columns(columns_by_name, identifier) do
      [column] -> column
      [_|_] -> raise CompilationError, message: "Column `#{column_name}` is ambiguous."
      nil ->
        columns_by_name
        |> Map.values()
        |> List.flatten()
        |> Enum.map(&(&1.table))
        |> Enum.uniq()
        |> case do
            [table] ->
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table.name}`."
            [_|_] ->
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in any of the selected tables."
          end
    end
  end
  defp identifier_to_column({:identifier, table, identifier = {_, column_name}}, columns_by_name, query) do
    if Enum.any?(query.selected_tables, &(&1.name == table)) do
      case get_columns(columns_by_name, identifier) do
        nil ->
          raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
        columns ->
          case Enum.find(columns, &insensitive_equal?(&1.table.name, table)) do
            nil ->
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
            column -> column
          end
      end
    else
      case get_columns(columns_by_name, {:unquoted, "#{table}.#{column_name}"}) do
        [column] -> column
        [_|_] -> raise CompilationError, message: "Column `#{table}.#{column_name}` is ambiguous."
        nil -> raise CompilationError, message: "Missing FROM clause entry for table `#{table}`."
      end
    end
  end
  defp identifier_to_column({:function, name, args} = function, _columns_by_name, query) do
    check_function_validity(function, query)
    case Function.return_type(function) do
      nil -> raise CompilationError, message: function_argument_error_message(function)
      type -> Expression.function(name, args, type, Function.has_attribute?(name, :aggregator))
    end
  end
  defp identifier_to_column({:parameter, index}, _columns_by_name, query) do
    param_value = if query.parameters != nil, do: Enum.at(query.parameters, index - 1).value
    param_type = Query.parameter_type(query, index)
    if param_type == :unknown, do: parameter_error(index)
    Expression.constant(param_type, param_value)
  end
  defp identifier_to_column({:constant, type, value}, _columns_by_name, _query), do:
    Expression.constant(type, value)
  defp identifier_to_column(other, _columns_by_name, _query), do: other

  defp get_columns(columns_by_name, {:unquoted, name}) do
    columns_by_name
    |> Enum.find(fn({key, _}) -> insensitive_equal?(name, key) end)
    |> case do
      nil -> nil
      {_, columns} -> columns
    end
  end
  defp get_columns(columns_by_name, {:quoted, name}), do: Map.get(columns_by_name, name)

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)

  def column_title({_identifier, :as, alias}, _selected_tables), do: alias
  def column_title({:function, {:cast, _}, _}, _selected_tables), do: "cast"
  def column_title({:function, {:bucket, _}, _}, _selected_tables), do: "bucket"
  def column_title({:function, name, _}, _selected_tables), do: name
  def column_title({:distinct, identifier}, selected_tables), do: column_title(identifier, selected_tables)
  # This is needed for data sources that support dotted names for fields (MongoDB)
  def column_title({:identifier, {:unquoted, table}, {:unquoted, column}}, selected_tables), do:
    if find_table(selected_tables, {:unquoted, table}) == nil, do: "#{table}.#{column}", else: column
  def column_title({:identifier, _table, {_, column}}, _selected_tables), do: column
  def column_title({:constant, _, _}, _selected_tables), do: ""
  def column_title({:parameter, _}, _selected_tables), do: ""

  defp censor_selected_uids(%Query{command: :select, subquery?: false} = query) do
    columns = for column <- query.columns, do:
      if is_uid_column?(column), do: Expression.constant(:text, :*), else: column
    %Query{query | columns: columns}
  end
  defp censor_selected_uids(query), do: query

  defp is_uid_column?(%Expression{aggregate?: true}), do: false
  defp is_uid_column?(column), do: [column] |> extract_columns() |> Enum.any?(& &1.user_id?)

  defp extract_columns(columns), do:
    get_in(columns, [Query.Lenses.leaf_expressions()])

  defp calculate_db_columns(query), do:
    Enum.reduce(select_expressions(query) ++ range_columns(query), query, &Query.add_db_column(&2, &1))

  defp range_columns(%{subquery?: true, emulated?: false}), do: []
  defp range_columns(%{ranges: ranges}), do: ranges |> Enum.map(&(&1.column)) |> extract_columns()

  defp select_expressions(%Query{command: :select, subquery?: true, emulated?: false} = query) do
    Enum.zip(query.column_titles, query.columns)
    |> Enum.map(fn({column_alias, column}) -> %Expression{column | alias: column_alias} end)
  end
  defp select_expressions(%Query{command: :select} = query) do
    # top-level query -> we,re fetching only columns, while other expressions (e.g. function calls)
    # will be resolved in the post-processing phase
    used_columns =
      query
      |> needed_columns()
      |> extract_columns()
      |> Enum.reject(& &1.constant?)
    [id_column(query) | used_columns]
  end

  defp needed_columns(query), do:
    query.columns ++
    query.group_by ++
    query.emulated_where ++
    query.having ++
    if query.emulated?, do: query.where, else: []

  defp id_column(query) do
    id_columns = all_id_columns_from_tables(query)
    if any_outer_join?(query.from),
      do: Expression.function("coalesce", id_columns),
      else: hd(id_columns)
  end

  defp all_id_columns_from_tables(%Query{command: :select, selected_tables: tables}) do
    Enum.map(tables, fn(table) ->
      user_id = table.user_id
      {_, type} = Enum.find(table.columns, fn ({name, _type}) -> insensitive_equal?(user_id, name) end)
      %Expression{table: table, name: user_id, type: type, user_id?: true}
    end)
  end

  defp any_outer_join?(table) when is_binary(table), do: false
  defp any_outer_join?({:subquery, _}), do: false
  defp any_outer_join?({:join, %{type: type}})
    when type in [:full_outer_join, :left_outer_join, :right_outer_join],
    do: true
  defp any_outer_join?({:join, join}),
    do: any_outer_join?(join.lhs) || any_outer_join?(join.rhs)

  defp join_conditions_scope_check(from) do
    do_join_conditions_scope_check(from, [])
  end

  defp do_join_conditions_scope_check({:join, join}, selected_tables) do
    selected_tables = do_join_conditions_scope_check(join.lhs, selected_tables)
    selected_tables = do_join_conditions_scope_check(join.rhs, selected_tables)

    Lens.each(
      Query.Lenses.conditions_terminals(),
      join.conditions,
      fn
        (%Cloak.Sql.Expression{table: %{name: table_name}, name: column_name}) ->
          scope_check(selected_tables, table_name, column_name)
        ({:identifier, table_name, {_, column_name}}) -> scope_check(selected_tables, table_name, column_name)
        (_) -> :ok
      end
    )

    Enum.each(join.conditions, &verify_where_clause/1)
    selected_tables
  end
  defp do_join_conditions_scope_check({:subquery, subquery}, selected_tables),
    do: [subquery.alias | selected_tables]
  defp do_join_conditions_scope_check(table_name, selected_tables) when is_binary(table_name),
    do: [table_name | selected_tables]

  defp scope_check(tables_in_scope, table_name, column_name) do
    case Enum.member?(tables_in_scope, table_name) do
      true -> :ok
      _ -> raise CompilationError, message: "Column `#{column_name}` of table `#{table_name}` is used out of scope."
    end
  end

  defp verify_limit(%Query{command: :select, limit: amount}) when amount <= 0, do:
    raise CompilationError, message: "`LIMIT` clause expects a positive value."
  defp verify_limit(%Query{command: :select, order_by: [], limit: amount}) when amount != nil, do:
    raise CompilationError, message: "Using the `LIMIT` clause requires the `ORDER BY` clause to be specified."
  defp verify_limit(query), do: query

  defp verify_offset(%Query{command: :select, offset: amount}) when amount < 0, do:
    raise CompilationError, message: "`OFFSET` clause expects a non-negative value."
  defp verify_offset(%Query{command: :select, order_by: [], offset: amount}) when amount > 0, do:
    raise CompilationError, message: "Using the `OFFSET` clause requires the `ORDER BY` clause to be specified."
  defp verify_offset(query), do: query

  defp verify_having(%Query{command: :select, group_by: [], having: [_|_]}), do:
    raise CompilationError, message: "Using the `HAVING` clause requires the `GROUP BY` clause to be specified."
  defp verify_having(%Query{command: :select, having: [_|_]} = query) do
    for {:comparison, column, _operator, target} <- query.having, do:
      for term <- [column, target], do:
        if individual_column?(term, query), do:
          raise CompilationError,
            message: "`HAVING` clause can not be applied over column #{Expression.display_name(term)}."
    query
  end
  defp verify_having(query), do: query

  defp verify_where_clauses(%Query{where: clauses = [_|_]} = query) do
    Enum.each(clauses, &verify_where_clause/1)
    clauses
    |> get_in([Query.Lenses.conditions_terminals()])
    |> Enum.filter(& &1.aggregate?)
    |> case do
      [] -> :ok
      [column | _rest] ->
        raise CompilationError, message:
          "Expression #{Expression.display_name(column)} is not valid in the `WHERE` clause."
    end
    query
  end
  defp verify_where_clauses(query), do: query

  defp verify_where_clause({:comparison, column_a, comparator, column_b}) do
    verify_where_clause_types(column_a, column_b)
    check_for_string_inequalities(comparator, column_b)
  end
  defp verify_where_clause({verb, column, _}) when verb in [:like, :ilike] do
    if column.type != :text do
      verb = verb |> to_string() |> String.upcase()
      raise CompilationError, message:
        "Column #{Expression.display_name(column)} of type `#{column.type}` cannot be used in a #{verb} expression."
    end
  end
  defp verify_where_clause({:not, clause}), do: verify_where_clause(clause)
  defp verify_where_clause(_), do: :ok

  defp verify_where_clause_types(column_a, column_b) do
    if not Expression.constant?(column_a) and not Expression.constant?(column_b) and column_a.type != column_b.type do
      raise CompilationError, message: "Column #{Expression.display_name(column_a)} of type `#{column_a.type}` and "
        <> "column #{Expression.display_name(column_b)} of type `#{column_b.type}` cannot be compared."
    end
  end

  defp check_for_string_inequalities(comparator, %Expression{type: :text}) when comparator in [:>, :>=, :<, :<=], do:
    raise CompilationError, message: "Inequalities on string values are currently not supported."
  defp check_for_string_inequalities(_, _), do: :ok


  # -------------------------------------------------------------------
  # Query emulation
  # -------------------------------------------------------------------

  defp emulated_expression?(expression), do:
    DataDecoder.needs_decoding?(expression) or Function.has_attribute?(expression, :emulated)

  defp emulated_expression_condition?(condition), do:
    Comparison.verb(condition) != :is and
    [condition]
    |> get_in([Query.Lenses.conditions_terminals()])
    |> Enum.any?(&emulated_expression?/1)

  defp set_emulation_flag(query), do: %Query{query | emulated?: needs_emulation?(query)}

  defp has_emulated_expressions?(query), do:
    (query.columns ++ query.group_by ++ query.having ++ query.where)
    |> get_in([Query.Lenses.all_expressions()])
    |> Enum.any?(&emulated_expression?/1)

  defp has_emulated_join_conditions?(query), do:
    all_join_conditions(query.from)
    |> get_in([Query.Lenses.all_expressions()])
    |> Enum.any?(&emulated_expression?/1)

  defp needs_emulation?(%Query{subquery?: false, from: table}) when is_binary(table), do: false
  defp needs_emulation?(%Query{subquery?: true, from: table} = query) when is_binary(table), do:
    not query.data_source.driver.supports_query?(query) or has_emulated_expressions?(query)
  defp needs_emulation?(query), do:
    not query.data_source.driver.supports_query?(query) or
    query |> get_in([Query.Lenses.direct_subqueries()]) |> Enum.any?(&(&1.ast.emulated?)) or
    (query.subquery? and has_emulated_expressions?(query)) or
    has_emulated_join_conditions?(query)


  # -------------------------------------------------------------------
  # Compiling of parameter types
  # -------------------------------------------------------------------

  defp compile_parameter_types(query), do:
    query
    |> resolve_parameter_types()
    |> check_missing_parameter_types()

  defp resolve_parameter_types(%Query{parameters: parameters} = query) when is_list(parameters), do:
    # Parameters are bound
    parameters
    |> Enum.with_index()
    |> Enum.reduce(query, fn({param, index}, query) -> Query.set_parameter_type(query, index + 1, param.type) end)
  defp resolve_parameter_types(%Query{parameters: nil} = query), do:
    # Parameters are not bound. This is possible if PostgreSQL client issues a describe command before it
    # binds parameters, which is allowed by the protocol. In this case, we'll derive parameter types from
    # cast expressions. In other words, with late binding the parameter must be explicitly casted, so we
    # can determine its type.
    query
    |> add_parameter_types_from_subqueries()
    |> add_parameter_types_from_casts()

  defp add_parameter_types_from_subqueries(query), do:
    Enum.reduce(
      get_in(query, [Query.Lenses.direct_subqueries()]),
      query,
      &Query.merge_parameter_types(&2, &1.ast)
    )

  defp add_parameter_types_from_casts(query), do:
    Enum.reduce(
      get_in(query, [Query.Lenses.raw_parameter_casts()]),
      query,
      fn({:function, {:cast, type}, [{:parameter, index}]}, query) ->
        Query.set_parameter_type(query, index, type)
      end
    )

  defp check_missing_parameter_types(%Query{subquery?: true} = query), do:
    query
  defp check_missing_parameter_types(%Query{subquery?: false} = query) do
    # This verifies whether there's a missing parameter type. This can happen if a late bound parameter has
    # not been casted, or if it's omitted from the query.
    case Enum.find_index(Query.parameter_types(query), &(&1 == :unknown)) do
      nil -> query
      index -> parameter_error(index + 1)
    end
  end

  defp parameter_error(parameter_index), do:
    raise(CompilationError, message: "The type for parameter `$#{parameter_index}` cannot be determined.")
end
