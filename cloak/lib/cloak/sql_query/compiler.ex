defmodule Cloak.SqlQuery.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.SqlQuery.Column
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token
  alias Cloak.SqlQuery.Function

  @type compiled_query :: %{
    data_source: DataSource.t,
    command: :select | :show,
    columns: [Column.t],
    column_titles: [String.t],
    property: [String.t],
    aggregators: [{String.t, String.t}],
    implicit_count: true,
    unsafe_filter_columns: [Column.t],
    group_by: [String.t],
    from: [String.t],
    where: [Parser.where_clause],
    where_not: [Parser.where_clause],
    order_by: [{pos_integer, :asc | :desc}],
    show: :tables | :columns,
    selected_tables: [String.t],
    db_columns: [Column.t]
  }

  defmodule CompilationError do
    @moduledoc false
    defexception message: "Error during compiling query"
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(atom, Parser.parsed_query) :: {:ok, compiled_query} | {:error, String.t}
  def compile(data_source, query) do
    defaults = %{data_source: data_source, where: [], where_not: [], unsafe_filter_columns: [],
      group_by: [], order_by: [], column_titles: [], info: [], selected_tables: [], db_columns: []}
    compile_prepped_query(Map.merge(defaults, query))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # Due to the blackbox nature of the subquery, there are a whole lot
  # of validations we cannot do when using DS proxy. Conversely, there
  # are also built in cloak functionality that we cannot provide, like
  # regular, top level WHERE clauses.
  # We therefore perform custom DS proxy validations, in order to
  # keep the remaining validations clean, and free from having to
  # consider the DS Proxy case.
  defp compile_prepped_query(%{command: :select, from: {:subquery, _}} = query) do
    try do
      ds_proxy_validate_no_wildcard(query)
      ds_proxy_validate_no_where(query)
      query = query
      |> compile_columns()
      |> verify_columns()
      |> compile_order_by()
      |> partition_selected_columns()
      |> calculate_db_columns()
      {:ok, query}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end
  defp compile_prepped_query(%{command: :show} = query) do
    try do
      {:ok, compile_tables(query)}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end
  defp compile_prepped_query(query) do
    try do
      query = query
      |> compile_tables()
      |> compile_columns()
      |> verify_columns()
      |> compile_order_by()
      |> verify_joins()
      |> cast_where_clauses()
      |> partition_selected_columns()
      |> partition_where_clauses()
      |> calculate_db_columns()
      {:ok, query}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # DS Proxy validators.
  # -------------------------------------------------------------------

  defp ds_proxy_validate_no_wildcard(%{command: :select, columns: :*}) do
    raise CompilationError, message: "Unfortunately wildcard selects are not supported together with subselects"
  end
  defp ds_proxy_validate_no_wildcard(_), do: :ok

  defp ds_proxy_validate_no_where(%{where: []}), do: :ok
  defp ds_proxy_validate_no_where(_) do
    raise CompilationError, message: "WHERE-clause in outer SELECT is not allowed in combination with a subquery"
  end


  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp compile_tables(%{from: _} = query) do
    selected_table_names = from_clause_to_tables(query.from)
    available_table_names = Enum.map(DataSource.tables(query.data_source), &Atom.to_string/1)

    case selected_table_names -- available_table_names do
      [] ->
        %{query |
            selected_tables: Enum.map(selected_table_names, &DataSource.table(query.data_source, &1))
        }

      [table | _] ->
        raise CompilationError, message: "Table `#{table}` doesn't exist."
    end
  end
  defp compile_tables(query), do: query

  defp from_clause_to_tables({:cross_join, table, rest}), do: [table | from_clause_to_tables(rest)]
  defp from_clause_to_tables(table), do: [table]

  defp compile_aliases(%{columns: [_|_] = columns} = query) do
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
    %{query | column_titles: column_titles, columns: columns, group_by: group_by, order_by: order_by}
  end
  defp compile_aliases(query), do: query

  # Subqueries can produce column-names that are not actually in the table. Without understanding what
  # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
  # the outer column selections
  defp verify_aliases(%{command: :select, from: {:subquery, _}}), do: :ok
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

  defp invalid_not_aggregated_columns(%{command: :select, group_by: [_|_]} = query) do
    query.columns
    |> Stream.reject(&Function.aggregate_function?/1)
    |> Stream.reject(fn(column) -> Enum.all?(dependency_columns(column), &Enum.member?(query.group_by, &1)) end)
    |> Enum.reject(fn(column) -> Enum.member?(query.group_by, column) end)
  end
  defp invalid_not_aggregated_columns(%{command: :select} = query) do
    case Enum.partition(query.columns, &Function.aggregate_function?/1) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  defp dependency_columns({:function, _, arguments}), do: Enum.flat_map(arguments, &dependency_columns/1)
  defp dependency_columns({:distinct, identifier}), do: [identifier]
  defp dependency_columns(identifier), do: [identifier]

  defp compile_columns(query) do
    query
    |> expand_star_select()
    |> compile_aliases()
    |> transform_columns()
  end

  defp expand_star_select(%{columns: :*} = query) do
    columns = all_column_identifiers(query)
    column_names = for {:identifier, _table, name} <- columns, do: name
    %{query | columns: columns, column_titles: column_names}
  end
  defp expand_star_select(query), do: query

  defp filter_aggregators(columns), do: Enum.filter(columns, &Function.aggregate_function?/1)

  defp verify_columns(query) do
    verify_functions(query)
    verify_aggregated_columns(query)
    verify_group_by_functions(query)
    verify_function_arguments(query)
    warn_on_selected_uids(query)
  end

  defp verify_function_arguments(query) do
    query.columns
    |> Enum.filter(&Function.function?/1)
    |> Enum.reject(&Function.well_typed?/1)
    |> case do
      [] -> :ok
      [function_call | _rest] ->
        expected_types = Function.argument_types(function_call)
        actual_types = Function.arguments(function_call) |> Enum.map(&(&1.type))

        raise CompilationError, message: "Function `#{Function.name(function_call)}`"
          <> " requires (#{quoted_list(expected_types)}), but got (#{quoted_list(actual_types)})"
    end
  end

  defp quoted_list(items), do:
    items |> Enum.map(&quoted_item/1) |> Enum.join(", ")

  defp quoted_item({:optional, type}), do: "[`#{type}`]"
  defp quoted_item(item), do: "`#{item}`"

  defp verify_aggregated_columns(query) do
    case invalid_not_aggregated_columns(query) do
      [] -> :ok
      [column | _rest] ->
        raise CompilationError, message: "#{aggregated_expression_display(column)} needs " <>
          "to appear in the `group by` clause or be used in an aggregate function."
    end
  end

  defp aggregated_expression_display({:function, _function, [column]}) do
    aggregated_expression_display(column)
  end
  defp aggregated_expression_display(%Column{} = column) do
    "Column `#{column.name}` from table `#{column.table.user_name}`"
  end

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
      {:identifier, table.user_name, column_name}
    end
  end

  defp partition_selected_columns(%{group_by: groups = [_|_], columns: columns} = query) do
    aggregators = filter_aggregators(columns)
    Map.merge(query, %{property: groups |> Enum.uniq(), aggregators: aggregators |> Enum.uniq()})
  end
  defp partition_selected_columns(%{columns: columns} = query) do
    aggregators = filter_aggregators(columns)
    partitioned_columns = case aggregators do
      [] -> %{property: columns |> Enum.uniq(), aggregators: [{:function, "count", [:*]}], implicit_count: true}
      _ -> %{property: [], aggregators: aggregators |> Enum.uniq()}
    end
    Map.merge(query, partitioned_columns)
  end
  defp partition_selected_columns(query), do: query

  defp compile_order_by(%{order_by: []} = query), do: query
  defp compile_order_by(%{columns: columns, order_by: order_by_spec} = query) do
    columns_by_name = columns_by_name(query)
    qualified_columns = Enum.map(columns, &convert_column(&1, columns_by_name, query))
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) ->
      Enum.member?(qualified_columns, convert_column(column, columns_by_name, query))
    end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          qualified_column = convert_column(column, columns_by_name, query)
          index = qualified_columns |> Enum.find_index(&(&1 == qualified_column))
          {index, direction}
        end
        %{query | order_by: order_list}
      [{column, _direction} | _rest] ->
        raise CompilationError, message:
          "Non-selected #{Column.display_name(column)} specified in `order by` clause."
    end
  end

  defp partition_where_clauses(%{where: clauses, where_not: [], unsafe_filter_columns: []} = query) do
    {positive, negative} = Enum.partition(clauses, fn
       {:not, {:is, _, :null}} -> true
       {:not, _} -> false
       _ -> true
    end)
    negative = Enum.map(negative, fn({:not, clause}) -> clause end)
    unsafe_filter_columns = Enum.map(negative, &where_clause_to_identifier/1)

    %{query | where: positive, where_not: negative, unsafe_filter_columns: unsafe_filter_columns}
  end
  defp partition_where_clauses(query), do: query

  defp verify_joins(query) do
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
      uid_columns = Enum.map(query.selected_tables, &%Column{name: &1.user_id, table: &1})
      Enum.each(uid_columns, &:digraph.add_vertex(graph, column_key.(&1)))

      # add edges for all `uid1 = uid2` filters
      for {:comparison, column1, :=, column2} <- query.where,
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
              table1 = column1.table.user_name
              table2 = column2.table.user_name
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

  defp cast_where_clauses(%{where: [_|_] = clauses} = query) do
    %{query | where: Enum.map(clauses, &cast_where_clause/1)}
  end
  defp cast_where_clauses(query), do: query

  defp cast_where_clause(clause) do
    column = where_clause_to_identifier(clause)
    do_cast_where_clause(clause, column.type)
  end

  defp do_cast_where_clause({:not, subclause}, type) do
    {:not, do_cast_where_clause(subclause, type)}
  end
  defp do_cast_where_clause({:comparison, identifier, comparator, rhs}, :timestamp) do
    {:comparison, identifier, comparator, parse_time(rhs)}
  end
  defp do_cast_where_clause({:in, column, values}, :timestamp) do
    {:in, column, Enum.map(values, &parse_time/1)}
  end
  defp do_cast_where_clause(clause, _), do: clause

  defp parse_time(%Token{category: :constant, value: %{type: :string, value: string}}) do
    case Timex.parse(string, "{ISO}") do
      {:ok, value} -> value
      _ -> case Timex.parse(string, "{ISOdate}") do
        {:ok, value} -> value
        _ -> raise CompilationError, message: "Cannot cast `#{string}` to timestamp."
      end
    end
  end
  defp parse_time(%Token{value: %{value: value}}) do
    raise CompilationError, message: "Cannot cast `#{value}` to timestamp."
  end

  defp where_clause_to_identifier({:comparison, identifier, _, _}), do: identifier
  defp where_clause_to_identifier({:not, subclause}), do: where_clause_to_identifier(subclause)
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp where_clause_to_identifier({unquote(keyword), identifier, _}), do: identifier
  end)

  defp transform_columns(%{from: {:subquery, _}} = query),
    do: transform_columns(query, nil)
  defp transform_columns(query),
    do: transform_columns(query, columns_by_name(query))

  defp transform_columns(query, columns_by_name) do
    %{query |
      columns: Enum.map(query.columns, &(convert_column(&1, columns_by_name, query))),
      group_by: Enum.map(query.group_by, &(convert_column(&1, columns_by_name, query))),
      where: Enum.map(query.where, &(qualify_where_clause(&1, columns_by_name, query))),
      where_not: Enum.map(query.where_not, &(qualify_where_clause(&1, columns_by_name, query))),
      order_by: Enum.map(query.order_by, fn({identifier, direction}) ->
        {convert_column(identifier, columns_by_name, query), direction}
      end)
    }
  end

  defp columns_by_name(query) do
    for table <- query.selected_tables, {column, type} <- table.columns do
      %Column{table: table, name: column, type: type, user_id?: table.user_id == column}
    end
    |> Enum.group_by(&(&1.name))
  end

  defp convert_column(column, nil, _query) do
    do_convert_column(column, &convert_column_for_unsafe_query(&1))
  end
  defp convert_column(column, columns_by_name, query) do
    do_convert_column(column, &convert_column_for_parsed_query(&1, columns_by_name, query))
  end

  defp convert_column_for_unsafe_query({:identifier, :unknown, column_name}) do
    %Column{name: column_name, table: :unknown}
  end

  defp convert_column_for_parsed_query({:identifier, :unknown, column_name}, columns_by_name, _query) do
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
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table.user_name}`."
            [_|_] ->
              raise CompilationError, message: "Column `#{column_name}` doesn't exist in any of the selected tables."
          end
    end
  end
  defp convert_column_for_parsed_query({:identifier, table, column_name}, columns_by_name, query) do
    unless Enum.any?(query.selected_tables, &(&1.user_name == table)),
      do: raise CompilationError, message: "Missing FROM clause entry for table `#{table}`"

    case Map.fetch(columns_by_name, column_name) do
      :error ->
        raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
      {:ok, columns} ->
        case Enum.find(columns, &(&1.table.user_name == table)) do
          nil ->
            raise CompilationError, message: "Column `#{column_name}` doesn't exist in table `#{table}`."
          column ->
            column
        end
    end
  end

  defp do_convert_column(%Token{} = token, _converter_fun), do: token
  defp do_convert_column(%Column{} = column, _converter_fun), do: column
  defp do_convert_column({:function, "count", [:*]} = function, _converter_fun), do: function
  defp do_convert_column({:function, function, arguments}, converter_fun), do:
    {:function, function, Enum.map(arguments, &do_convert_column(&1, converter_fun))}
  defp do_convert_column({:distinct, identifier}, converter_fun), do:
    {:distinct, do_convert_column(identifier, converter_fun)}
  defp do_convert_column({:identifier, _, _} = identifier, converter_fun), do:
    converter_fun.(identifier)
  defp do_convert_column({:constant, %Token{value: %{type: type, value: value}}}, _converter_fun), do:
    Column.constant(type, value)

  defp qualify_where_clause({:comparison, lhs, comparator, rhs}, columns_by_name, query) do
    {
      :comparison,
      convert_column(lhs, columns_by_name, query),
      comparator,
      convert_column(rhs, columns_by_name, query)
    }
  end
  defp qualify_where_clause({:not, subclause}, columns_by_name, query) do
    {:not, qualify_where_clause(subclause, columns_by_name, query)}
  end
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp qualify_where_clause({unquote(keyword), identifier, any}, columns_by_name, query) do
      {unquote(keyword), convert_column(identifier, columns_by_name, query), any}
    end
  end)

  def column_title({:function, function, _}), do: function
  def column_title({:distinct, identifier}), do: column_title(identifier)
  def column_title({:identifier, _table, column}), do: column
  def column_title({:constant, _}), do: ""

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
  defp extract_columns({:function, _function, arguments}), do: Enum.flat_map(arguments, &extract_columns/1)
  defp extract_columns({:distinct, expression}), do: extract_columns(expression)

  defp warn_on_selected_uid(query, column) do
    add_info_message(query,
      "Selecting #{Column.display_name(column)} will cause all values to be anonymized. " <>
      "Consider removing this column from the list of selected columns."
    )
  end

  defp add_info_message(query, info_message), do: %{query | info: [info_message | query.info]}

  defp calculate_db_columns(query) do
    %{query |
        db_columns:
          db_columns(query)
          |> Enum.flat_map(&extract_columns/1)
          |> Enum.reject(&(&1 == nil))
          |> Enum.reject(&(&1.constant?))
          |> Enum.uniq()
    }
  end

  defp db_columns(%{command: :select, from: {:subquery, _}} = query) do
    [%Column{table: :unknown, name: "user_id", user_id?: true}] ++ query.columns ++ query.group_by
  end
  defp db_columns(%{command: :select, selected_tables: [table | _]} = query) do
    user_id = table.user_id
    {_, type} = Enum.find(table.columns, &match?({^user_id, _}, &1))
    user_id_column = %Column{table: table, name: user_id, type: type, user_id?: true}
    [user_id_column] ++ query.columns ++ query.group_by ++ query.unsafe_filter_columns
  end
end
