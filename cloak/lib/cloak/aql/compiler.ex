defmodule Cloak.Aql.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.Aql.Query
  alias Cloak.Aql.Column
  alias Cloak.Aql.Parser
  alias Cloak.Aql.Parsers.Token
  alias Cloak.Aql.Function

  defmodule CompilationError do
    @moduledoc false
    defexception message: "Error during compiling query"
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(DataSource.t, Parser.parsed_query) :: {:ok, Query.t} | {:error, String.t}
  def compile(data_source, parsed_query) do
    try do
      parsed_query
      |> to_prepped_query(data_source)
      |> compile_prepped_query()
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_prepped_query(parsed_query, data_source) do
    %Query{
      data_source: data_source,
      mode: query_mode(data_source.driver, parsed_query[:from])
    }
    |> Map.merge(parsed_query)
  end

  defp query_mode(Cloak.DataSource.DsProxy, {:subquery, %{type: :unparsed}}), do: :unparsed
  defp query_mode(Cloak.DataSource.DsProxy, from) do
    validate_dsproxy_from_for_parsed_query!(from)
    :parsed
  end
  defp query_mode(_other_data_source, _from), do: :parsed

  defp validate_dsproxy_from_for_parsed_query!(nil), do: :ok
  defp validate_dsproxy_from_for_parsed_query!({:join, join}) do
    validate_dsproxy_from_for_parsed_query!(join.lhs)
    validate_dsproxy_from_for_parsed_query!(join.rhs)
  end
  defp validate_dsproxy_from_for_parsed_query!({:subquery, _}) do
    raise CompilationError, message: "Joining subqueries is not supported for this data source"
  end
  defp validate_dsproxy_from_for_parsed_query!(table_name) when is_binary(table_name), do: :ok

  # Due to the blackbox nature of the subquery, there are a whole lot
  # of validations we cannot do when using DS proxy. Conversely, there
  # are also built in cloak functionality that we cannot provide, like
  # regular, top level WHERE clauses.
  # We therefore perform custom DS proxy validations, in order to
  # keep the remaining validations clean, and free from having to
  # consider the DS Proxy case.
  defp compile_prepped_query(%Query{command: :select, mode: :unparsed} = query) do
    ds_proxy_validate_no_wildcard(query)
    ds_proxy_validate_no_where(query)
    query = query
    |> compile_columns()
    |> verify_columns()
    |> compile_order_by()
    |> partition_selected_columns()
    |> calculate_db_columns()
    |> verify_limit()
    |> verify_offset()
    {:ok, query}
  end
  defp compile_prepped_query(%Query{command: :show} = query) do
    try do
      {:ok, compile_tables(query)}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end
  defp compile_prepped_query(query) do
    try do
      query = query
      |> compile_subqueries()
      |> compile_tables()
      |> compile_columns()
      |> verify_columns()
      |> compile_order_by()
      |> verify_joins()
      |> cast_where_clauses()
      |> partition_selected_columns()
      |> partition_where_clauses()
      |> calculate_db_columns()
      |> verify_limit()
      |> verify_offset()
      {:ok, query}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # DS Proxy validators.
  # -------------------------------------------------------------------

  defp ds_proxy_validate_no_wildcard(%Query{command: :select, columns: :*}) do
    raise CompilationError, message: "Unfortunately wildcard selects are not supported together with subselects"
  end
  defp ds_proxy_validate_no_wildcard(_), do: :ok

  defp ds_proxy_validate_no_where(%Query{where: []}), do: :ok
  defp ds_proxy_validate_no_where(_) do
    raise CompilationError, message: "WHERE-clause in outer SELECT is not allowed in combination with a subquery"
  end


  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp compile_subqueries(%Query{from: nil} = query), do: query
  defp compile_subqueries(query) do
    %Query{query | from: do_compile_subqueries(query.from, query.data_source)}
  end

  defp do_compile_subqueries({:join, join}, data_source) do
    {:join, %{join |
      lhs: do_compile_subqueries(join.lhs, data_source),
      rhs: do_compile_subqueries(join.rhs, data_source)
    }}
  end
  defp do_compile_subqueries({:subquery, subquery}, data_source) do
    {:subquery, %{subquery | ast: compiled_subquery(data_source, subquery.ast)}}
  end
  defp do_compile_subqueries(table, _data_source) when is_binary(table), do: table

  defp compiled_subquery(data_source, parsed_query) do
    case compile(data_source, Map.put(parsed_query, :subquery?, :true)) do
      {:ok, compiled_query} -> validate_subquery(compiled_query)
      {:error, error} -> raise CompilationError, message: error
    end
  end

  defp validate_subquery(subquery) do
    case Enum.find(subquery.db_columns, &(&1.user_id?)) do
      nil ->
        possible_uid_columns =
          all_id_columns_from_tables(subquery)
          |> Enum.map(&Column.display_name/1)
          |> Enum.join(", ")

        raise CompilationError, message:
          "Missing a user id column in the select list of a subquery. " <>
          "To fix this error, add one of #{possible_uid_columns} to the subquery select list."
      _ ->
        subquery
    end
  end


  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp compile_tables(%Query{from: nil} = query), do: query
  defp compile_tables(query) do
    %Query{query | selected_tables: selected_tables(query.from, query.data_source)}
  end

  defp selected_tables({:join, join}, data_source) do
    selected_tables(join.lhs, data_source) ++ selected_tables(join.rhs, data_source)
  end
  defp selected_tables({:subquery, subquery}, _data_source) do
    [%{
      name: subquery.alias,
      db_name: nil,
      columns: Enum.map(subquery.ast.db_columns, &{&1.alias || &1.name, &1.type}),
      user_id: Enum.find(subquery.ast.db_columns, &(&1.user_id?)).name
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
    column_titles = Enum.map(columns, fn
      ({_column, :as, name}) -> name
      (column) -> column_title(column)
    end)
    aliases = (for {column, :as, name} <- columns, do: {{:identifier, :unknown, name}, column}) |> Enum.into(%{})
    columns = Enum.map(columns, fn
      ({column, :as, _name}) -> column
      (column) -> column
    end)
    order_by = for {column, direction} <- query.order_by, do: {Map.get(aliases, column, column), direction}
    group_by = for identifier <- query.group_by, do: Map.get(aliases, identifier, identifier)
    %Query{query | column_titles: column_titles, columns: columns, group_by: group_by, order_by: order_by}
  end
  defp compile_aliases(query), do: query

  # Subqueries can produce column-names that are not actually in the table. Without understanding what
  # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
  # the outer column selections
  defp verify_aliases(%Query{command: :select, mode: :unparsed}), do: :ok
  defp verify_aliases(query) do
    aliases = for {_column, :as, name} <- query.columns, do: name
    all_identifiers = aliases ++ all_column_identifiers(query)
    referenced_names = (for {{:identifier, _table, name}, _direction} <- query.order_by, do: name) ++
      query.group_by
    ambiguous_names = for name <- referenced_names, Enum.count(all_identifiers, &name == &1) > 1, do: name
    case ambiguous_names do
      [] -> :ok
      [name | _rest] -> raise CompilationError, message: "Usage of `#{name}` is ambiguous."
    end
  end

  defp invalid_not_aggregated_columns(%Query{command: :select, group_by: [_|_]} = query) do
    query.columns
    |> Stream.reject(&aggregated_column?(&1, query))
    |> Enum.reject(fn(column) -> Enum.member?(query.group_by, column) end)
  end
  defp invalid_not_aggregated_columns(%Query{command: :select} = query) do
    case Enum.partition(query.columns, &aggregated_column?(&1, query)) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  defp aggregated_column?(column, query) do
    Column.constant?(column) ||
      Function.aggregate_function?(column) ||
      Column.aggregate_db_function?(column) ||
      Enum.member?(query.group_by, column) ||
      (Function.function?(column) && Enum.all?(Function.arguments(column), &aggregated_column?(&1, query)))
  end

  defp compile_columns(query) do
    query
    |> expand_star_select()
    |> compile_aliases()
    |> identifiers_to_columns()
  end

  defp expand_star_select(%Query{columns: :*} = query) do
    columns = all_column_identifiers(query)
    column_names = for {:identifier, _table, name} <- columns, do: name
    %Query{query | columns: columns, column_titles: column_names}
  end
  defp expand_star_select(query), do: query

  defp filter_aggregators(columns), do:
    columns
    |> Enum.flat_map(&expand_arguments/1)
    |> Enum.filter(&Function.aggregate_function?/1)

  defp verify_columns(query) do
    verify_functions(query)
    verify_aggregated_columns(query)
    verify_group_by_functions(query)
    verify_function_arguments(query)
    warn_on_selected_uids(query)
  end

  defp verify_function_arguments(%Query{mode: :unparsed}), do: :ok
  defp verify_function_arguments(query) do
    query.columns
    |> Enum.flat_map(&expand_arguments/1)
    |> Enum.reject(&Function.well_typed?/1)
    |> case do
      [] -> :ok
      [function_call | _rest] ->
        raise CompilationError, message: function_argument_error_message(function_call)
    end
  end

  defp function_argument_error_message(function_call) do
    cond do
      Function.cast?(function_call) ->
        [cast_source] = actual_types(function_call)
        cast_target = Function.return_type(function_call)
        "Cannot cast value of type `#{cast_source}` to type `#{cast_target}`."
      many_overloads?(function_call) ->
        "Arguments of type (#{function_call |> actual_types() |> quoted_list()}) are incorrect"
          <> " for `#{Function.name(function_call)}`"
      true ->
        "Function `#{Function.name(function_call)}` requires arguments of type #{expected_types(function_call)}"
          <> ", but got (#{function_call |> actual_types() |> quoted_list()})"
    end
  end

  defp many_overloads?(function_call) do
    length(Function.argument_types(function_call)) > 4
  end

  defp expected_types(function_call), do:
    Function.argument_types(function_call)
    |> Enum.map(&quoted_list/1)
    |> Enum.map(&"(#{&1})")
    |> Enum.join(" or ")

  defp actual_types(function_call), do:
    Function.arguments(function_call) |> Enum.map(&Function.type/1)

  defp expand_arguments(column) do
    (column |> Function.arguments() |> Enum.flat_map(&expand_arguments/1)) ++ [column]
  end

  defp quoted_list(items), do:
    items |> Enum.map(&quoted_item/1) |> Enum.join(", ")

  defp quoted_item({:optional, type}), do: "[`#{type}`]"
  defp quoted_item({:many1, type}), do: "[`#{type}`]+"
  defp quoted_item({:or, types}), do: types |> Enum.map(&quoted_item/1) |> Enum.join(" | ")
  defp quoted_item(item), do: "`#{item}`"

  defp verify_aggregated_columns(query) do
    case invalid_not_aggregated_columns(query) do
      [] -> :ok
      [column | _rest] ->
        raise CompilationError, message: "#{aggregated_expression_display(column)} " <>
          "to appear in the `group by` clause or be used in an aggregate function."
    end
  end

  defp aggregated_expression_display({:function, _function, args}), do:
    "Columns (#{args |> Enum.map(&(&1.name)) |> quoted_list()}) need"
  defp aggregated_expression_display(%Column{} = column), do:
    "Column `#{column.name}` from table `#{column.table.name}` needs"

  defp verify_group_by_functions(query) do
    query.group_by
    |> Enum.filter(&Function.aggregate_function?/1)
    |> case do
      [] -> :ok
      [function | _] -> raise CompilationError, message: "Aggregate function `#{Function.name(function)}`"
        <> " used in the group by clause"
    end
  end

  defp verify_functions(query) do
    query.columns
    |> Enum.filter(&Function.function?/1)
    |> Enum.reject(&Function.valid_function?/1)
    |> case do
      [] -> :ok
      [{:function, invalid_function, _} | _rest] ->
        raise CompilationError, message: ~s/Unknown function `#{invalid_function}`./
    end
  end

  defp all_column_identifiers(query) do
    for table <- query.selected_tables, {column_name, _type} <- table.columns do
      {:identifier, table.name, column_name}
    end
  end

  defp partition_selected_columns(%Query{subquery?: true} = query), do: query
  defp partition_selected_columns(%Query{group_by: groups = [_|_], columns: columns} = query) do
    aggregators = filter_aggregators(columns)
    %Query{query | property: groups |> Enum.uniq(), aggregators: aggregators |> Enum.uniq()}
  end
  defp partition_selected_columns(%Query{columns: columns} = query) do
    case filter_aggregators(columns) do
      [] ->
        %Query{query |
          property: columns |> Enum.uniq(), aggregators: [{:function, "count", [:*]}], implicit_count: true
        }
      aggregators ->
        %Query{query | property: [], aggregators: aggregators |> Enum.uniq()}
    end
  end
  defp partition_selected_columns(query), do: query

  defp compile_order_by(%Query{order_by: []} = query), do: query
  defp compile_order_by(%Query{columns: columns, order_by: order_by_spec} = query) do
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) ->
      Enum.member?(columns, column)
    end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          index = columns |> Enum.find_index(&(&1 == column))
          {index, direction}
        end
        %Query{query | order_by: order_list}
      [{_column, _direction} | _rest] ->
        raise CompilationError, message:
          "Non-selected column specified in `order by` clause."
    end
  end

  defp partition_where_clauses(%Query{subquery?: true} = query) do
    case Enum.find(query.where, &negative_condition?/1) do
      nil -> query
      negative_condition ->
        raise CompilationError,
          message: "#{negative_condition_string(negative_condition)} is not supported in a subquery."
    end
  end
  defp partition_where_clauses(query) do
    {negative, positive} = Enum.partition(query.where, &negative_condition?/1)
    negative = Enum.map(negative, fn({:not, clause}) -> clause end)
    unsafe_filter_columns = Enum.map(negative, &where_clause_to_identifier/1)

    %Query{query | where: positive, where_not: negative, unsafe_filter_columns: unsafe_filter_columns}
  end

  defp negative_condition?({:not, {:is, _, :null}}), do: false
  defp negative_condition?({:not, _other}), do: true
  defp negative_condition?(_other), do: false

  defp verify_joins(query) do
    join_conditions_scope_check(query.from)
    Enum.each(all_join_conditions(query.from), &verify_supported_join_condition/1)

    # Algorithm for finding improperly joined tables:
    #
    # 1. Create a DCG graph, where all uid columns are vertices.
    # 2. Add an edge for all where clauses shaped as `uid1 = uid2`
    # 3. Find the first pair (uid1, uid2) where there is no path from uid1 to uid2 in the graph.
    # 4. Report an error if something is found in the step 3

    column_key = fn(column) -> {column.name, column.table.db_name} end

    graph = :digraph.new([:private, :cyclic])
    try do
      # add uid columns as vertices
      uid_columns = Enum.map(query.selected_tables, &%Column{name: &1.user_id, table: &1})
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

  defp cast_where_clauses(%Query{where: [_|_] = clauses} = query) do
    %Query{query | where: Enum.map(clauses, &cast_where_clause/1)}
  end
  defp cast_where_clauses(query), do: query

  defp cast_where_clause(clause) do
    column = where_clause_to_identifier(clause)
    do_cast_where_clause(clause, column.type)
  end

  @castable_conditions [:datetime, :time, :date]

  defp do_cast_where_clause({:not, subclause}, type) do
    {:not, do_cast_where_clause(subclause, type)}
  end
  defp do_cast_where_clause({:comparison, identifier, comparator, rhs}, type) when type in @castable_conditions do
    {:comparison, identifier, comparator, parse_time(rhs, type)}
  end
  defp do_cast_where_clause({:in, column, values}, type) when type in @castable_conditions do
    {:in, column, Enum.map(values, &parse_time(&1, type))}
  end
  defp do_cast_where_clause(clause, _), do: clause

  defp parse_time(column = %Column{constant?: true, value: string}, type) do
    case do_parse_time(column, type) do
      {:ok, result} -> result
      _ -> raise CompilationError, message: "Cannot cast `#{string}` to #{type}."
    end
  end

  defp do_parse_time(%Column{type: :text, value: string}, :date), do:
    Cloak.Time.parse_date(string)
  defp do_parse_time(%Column{type: :text, value: string}, :time), do:
    Cloak.Time.parse_time(string)
  defp do_parse_time(%Column{type: :text, value: string}, :datetime), do:
    Cloak.Time.parse_datetime(string)
  defp do_parse_time(_, _), do: {:error, :invalid_cast}

  defp where_clause_to_identifier({:comparison, identifier, _, _}), do: identifier
  defp where_clause_to_identifier({:not, subclause}), do: where_clause_to_identifier(subclause)
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp where_clause_to_identifier({unquote(keyword), identifier, _}), do: identifier
  end)

  defp map_terminal_elements(query, mapper_fun) do
    %Query{query |
      columns: Enum.map(query.columns, &map_terminal_element(&1, mapper_fun)),
      group_by: Enum.map(query.group_by, &map_terminal_element(&1, mapper_fun)),
      where: Enum.map(query.where, &map_where_clause(&1, mapper_fun)),
      where_not: Enum.map(query.where_not, &map_where_clause(&1, mapper_fun)),
      order_by: Enum.map(query.order_by, &map_order_by(&1, mapper_fun)),
      db_columns: Enum.map(query.db_columns, &map_terminal_element(&1, mapper_fun)),
      property: Enum.map(query.property, &map_terminal_element(&1, mapper_fun)),
      aggregators: Enum.map(query.aggregators, &map_terminal_element(&1, mapper_fun)),
      from: map_join_conditions_columns(query.from, mapper_fun)
    }
  end

  defp map_join_conditions_columns({:join, join}, mapper_fun) do
    {:join, %{join |
      lhs: map_join_conditions_columns(join.lhs, mapper_fun),
      rhs: map_join_conditions_columns(join.rhs, mapper_fun),
      conditions: Enum.map(join.conditions, &map_where_clause(&1, mapper_fun))
    }}
  end
  defp map_join_conditions_columns({:subquery, _} = subquery, _mapper_fun), do: subquery
  defp map_join_conditions_columns(raw_table_name, _mapper_fun) when is_binary(raw_table_name),
    do: raw_table_name

  defp map_where_clause({:comparison, lhs, comparator, rhs}, mapper_fun) do
    {
      :comparison,
      map_terminal_element(lhs, mapper_fun),
      comparator,
      map_terminal_element(rhs, mapper_fun)
    }
  end
  defp map_where_clause({:not, subclause}, mapper_fun) do
    {:not, map_where_clause(subclause, mapper_fun)}
  end
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp map_where_clause({unquote(keyword), lhs, rhs}, mapper_fun) do
      {unquote(keyword), map_terminal_element(lhs, mapper_fun), map_terminal_element(rhs, mapper_fun)}
    end
  end)

  defp map_order_by({identifier, direction}, mapper_fun),
    do: {map_terminal_element(identifier, mapper_fun), direction}

  defp map_terminal_element(%Token{} = token, mapper_fun), do: mapper_fun.(token)
  defp map_terminal_element(%Column{} = column, mapper_fun), do: mapper_fun.(column)
  defp map_terminal_element({:identifier, _, _} = identifier, mapper_fun), do: mapper_fun.(identifier)
  defp map_terminal_element({:function, "count", :*} = function, _converter_fun), do: function
  defp map_terminal_element({:function, "count_noise", :*} = function, _converter_fun), do: function
  defp map_terminal_element({:function, function, identifier}, converter_fun),
    do: converter_fun.({:function, function, map_terminal_element(identifier, converter_fun)})
  defp map_terminal_element({:distinct, identifier}, converter_fun),
    do: converter_fun.({:distinct, map_terminal_element(identifier, converter_fun)})
  defp map_terminal_element(elements, mapper_fun) when is_list(elements),
    do: Enum.map(elements, &map_terminal_element(&1, mapper_fun))
  defp map_terminal_element(constant, mapper_fun), do: mapper_fun.(constant)

  defp identifiers_to_columns(query) do
    columns_by_name =
      for table <- query.selected_tables, {column, type} <- table.columns do
        %Column{table: table, name: column, type: type, user_id?: table.user_id == column}
      end
      |> Enum.group_by(&(&1.name))

    map_terminal_elements(query, &(identifier_to_column(&1, columns_by_name, query)))
  end

  defp identifier_to_column({:identifier, :unknown, column_name}, _columns_by_name, %Query{mode: :unparsed}),
    do: %Column{name: column_name, table: :unknown}
  defp identifier_to_column({:identifier, :unknown, column_name}, columns_by_name, _query) do
    case Map.get(columns_by_name, column_name) do
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
  defp identifier_to_column({:identifier, table, column_name}, columns_by_name, query) do
    unless Enum.any?(query.selected_tables, &(&1.name == table)),
      do: raise CompilationError, message: "Missing FROM clause entry for table `#{table}`"

    case Map.fetch(columns_by_name, column_name) do
      :error ->
        raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
      {:ok, columns} ->
        case Enum.find(columns, &(&1.table.name == table)) do
          nil ->
            raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
          column -> column
        end
    end
  end
  defp identifier_to_column({:function, name, args} = function_spec, _columns_by_name, %Query{subquery?: true}) do
    case Function.return_type(function_spec) do
      nil -> raise CompilationError, message: function_argument_error_message(function_spec)
      type -> Column.db_function(name, args, type, Function.aggregate_function?(function_spec))
    end
  end
  defp identifier_to_column({:constant, type, value}, _columns_by_name, _query), do:
    Column.constant(type, value)
  defp identifier_to_column(other, _columns_by_name, _query), do: other

  def column_title(function = {:function, _, _}), do: Function.name(function)
  def column_title({:distinct, identifier}), do: column_title(identifier)
  def column_title({:identifier, _table, column}), do: column
  def column_title({:constant, _, _}), do: ""

  defp warn_on_selected_uids(query) do
    case selected_uid_columns(query) do
      [] -> query
      [_|_] = selected_uid_columns ->
        Enum.reduce(selected_uid_columns, query, &warn_on_selected_uid(&2, &1))
    end
  end

  defp selected_uid_columns(query) do
    query.columns
    |> Enum.flat_map(&extract_columns/1)
    |> Enum.filter(&(&1 != nil and &1.user_id?))
  end

  defp extract_columns(%Column{} = column), do: [column]
  defp extract_columns({:function, "count", [:*]}), do: [nil]
  defp extract_columns({:function, "count_noise", [:*]}), do: [nil]
  defp extract_columns({:function, _function, arguments}), do: Enum.flat_map(arguments, &extract_columns/1)
  defp extract_columns({:distinct, expression}), do: extract_columns(expression)
  defp extract_columns([columns]), do: Enum.flat_map(columns, &extract_columns(&1))

  defp warn_on_selected_uid(query, column) do
    add_info_message(query,
      "Selecting #{Column.display_name(column)} will cause all values to be anonymized. " <>
      "Consider removing this column from the list of selected columns."
    )
  end

  defp add_info_message(query, info_message), do: %Query{query | info: [info_message | query.info]}

  defp calculate_db_columns(query) do
    query = %Query{query |
      db_columns:
        query
        |> select_expressions()
        |> Enum.uniq_by(&db_column_name/1)
    }

    map_terminal_elements(query, &set_column_db_row_position(&1, query))
  end

  defp select_expressions(%Query{command: :select, subquery?: false} = query) do
    # top-level query -> we,re fetching only columns, while other expressions (e.g. function calls)
    # will be resolved in the post-processing phase
    [id_column(query) | query.columns ++ query.group_by ++ query.unsafe_filter_columns]
    |> Enum.flat_map(&extract_columns/1)
    |> Enum.reject(&(&1 == nil))
    |> Enum.reject(&(&1.constant?))
  end
  defp select_expressions(%Query{command: :select, subquery?: true} = query) do
    # currently we don't support functions in subqueries
    true = Enum.all?(query.columns, &match?(%Column{}, &1))
    Enum.zip(query.column_titles, query.columns)
    |> Enum.map(fn({column_alias, column}) -> %Column{column | alias: column_alias} end)
  end

  defp id_column(query) do
    all_id_columns = all_id_columns_from_tables(query) |> Enum.map(&cast_unknown_id/1)
    if any_outer_join?(query.from),
      do: Column.db_function("coalesce", all_id_columns),
      else: hd(all_id_columns)
  end

  # We can't directly select a field with an unknown type, so convert it to binary
  # This is needed in the case of using the ODBC driver with a GUID user id,
  # as the GUID type is not supported by the Erlang ODBC library
  def cast_unknown_id(%Column{type: :unknown} = column), do: Column.db_function({:cast, :varbinary}, [column])
  def cast_unknown_id(column), do: column

  defp all_id_columns_from_tables(%Query{command: :select, mode: :unparsed}) do
    # We don't know the name of the user_id column for an unsafe query, so we're generating
    # a fake one instead.
    [%Column{table: :unknown, name: "__aircloak_user_id__", user_id?: true}]
  end
  defp all_id_columns_from_tables(%Query{command: :select, selected_tables: tables}) do
    Enum.map(tables, fn(table) ->
      user_id = table.user_id
      {_, type} = Enum.find(table.columns, &match?({^user_id, _}, &1))
      %Column{table: table, name: user_id, type: type, user_id?: true}
    end)
  end

  defp any_outer_join?(table) when is_binary(table), do: false
  defp any_outer_join?({:subquery, _}), do: false
  defp any_outer_join?({:join, %{type: type}})
    when type in [:full_outer_join, :left_outer_join, :right_outer_join],
    do: true
  defp any_outer_join?({:join, join}),
    do: any_outer_join?(join.lhs) || any_outer_join?(join.rhs)

  defp set_column_db_row_position(%Column{user_id?: true} = column, _query) do
    # the user-id columns will collectively all be available in the first position
    %Column{column | db_row_position: 0}
  end
  defp set_column_db_row_position(%Column{} = column, %Query{db_columns: db_columns}) do
    case Enum.find_index(db_columns, &(db_column_name(&1) == db_column_name(column))) do
      # It's not actually a selected column, so ignore for the purpose of positioning
      nil -> column
      position -> %Column{column | db_row_position: position}
    end
  end
  defp set_column_db_row_position(other, _query), do: other

  defp db_column_name(%Column{table: :unknown, name: name}), do: name
  defp db_column_name(column), do: "#{column.table.db_name}.#{column.name}"

  defp join_conditions_scope_check(from) do
    do_join_conditions_scope_check(from, [])
  end

  defp do_join_conditions_scope_check({:join, join}, selected_tables) do
    selected_tables = do_join_conditions_scope_check(join.lhs, selected_tables)
    selected_tables = do_join_conditions_scope_check(join.rhs, selected_tables)
    mapper_fun = fn
      (%Cloak.Aql.Column{table: %{name: table_name}, name: column_name}) ->
        scope_check(selected_tables, table_name, column_name)
      ({:identifier, table_name, column_name}) -> scope_check(selected_tables, table_name, column_name)
      (_) -> :ok
    end
    Enum.each(join.conditions, &map_where_clause(&1, mapper_fun))
    selected_tables
  end
  defp do_join_conditions_scope_check({:subquery, subquery}, selected_tables),
    do: [subquery.alias | selected_tables]
  defp do_join_conditions_scope_check(table_name, selected_tables) when is_binary(table_name),
    do: [table_name | selected_tables]

  defp scope_check(tables_in_scope, table_name, column_name) do
    case Enum.member?(tables_in_scope, table_name) do
      true -> :ok
      _ ->
        raise CompilationError,
          message: "Column `#{column_name}` of table `#{table_name}` is used out of scope."
    end
  end

  defp verify_supported_join_condition(join_condition) do
    if negative_condition?(join_condition), do: raise CompilationError,
      message: "#{negative_condition_string(join_condition)} not supported in joins."
  end

  defp negative_condition_string({:not, {:like, _, _}}), do: "NOT LIKE"
  defp negative_condition_string({:not, {:ilike, _, _}}), do: "NOT ILIKE"
  defp negative_condition_string({:not, {:comparison, _, :=, _}}), do: "<>"

  defp verify_limit(%Query{command: :select, limit: amount}) when amount <= 0, do:
    raise CompilationError, message: "LIMIT clause expects a positive value."
  defp verify_limit(%Query{command: :select, order_by: [], limit: amount}) when amount != nil, do:
    raise CompilationError, message: "LIMIT clause needs the ORDER BY clause to be specified."
  defp verify_limit(query), do: query

  defp verify_offset(%Query{command: :select, offset: amount}) when amount < 0, do:
    raise CompilationError, message: "OFFSET clause expects a non-negative value."
  defp verify_offset(%Query{command: :select, order_by: [], offset: amount}) when amount > 0, do:
    raise CompilationError, message: "OFFSET clause needs the ORDER BY clause to be specified."
  defp verify_offset(query), do: query
end
