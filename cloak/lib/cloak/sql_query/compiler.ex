defmodule Cloak.SqlQuery.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token
  alias Cloak.SqlQuery.Function

  @type compiled_query :: %{
    data_source: DataSource.t,
    command: :select | :show,
    columns: [Parser.column],
    column_titles: [String.t],
    property: [String.t],
    aggregators: [{String.t, String.t}],
    implicit_count: true,
    unsafe_filter_columns: [Parser.column],
    group_by: [String.t],
    from: [String.t],
    where: [Parser.where_clause],
    where_not: [Parser.where_clause],
    order_by: [{pos_integer, :asc | :desc}],
    show: :tables | :columns,
    selected_tables: [String.t]
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
      group_by: [], order_by: [], column_titles: [], info: [], selected_tables: []}
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
      query = compile_aliases(query)
      verify_aggregated_columns(query)
      verify_group_by_functions(query)
      query = query
      |> compile_order_by()
      |> partition_selected_columns()
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
        Enum.map(selected_table_names, &DataSource.table(query.data_source, &1))
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
    column_names = for {identifier, _type} <- all_available_columns(query), do: identifier
    existing_names = aliases ++ column_names
    referenced_names = (for {{:identifier, _table, name}, _direction} <- query.order_by, do: name) ++
      query.group_by
    ambiguous_names = for name <- referenced_names, Enum.count(existing_names, &name == &1) > 1, do: name
    case ambiguous_names do
      [] -> :ok
      [name | _rest] -> raise CompilationError, message: "Usage of `#{name}` is ambiguous."
    end
  end

  defp invalid_not_aggregated_columns(%{command: :select, group_by: [_|_]} = query) do
    query.columns
    |> Stream.reject(&Function.aggregate_function?/1)
    |> Stream.reject(fn(column) -> Enum.any?(query.group_by, &(&1 == select_clause_to_identifier(column))) end)
    |> Stream.reject(fn(column) -> Enum.any?(query.group_by, &(select_clause_to_identifier(&1) == column)) end)
    |> Enum.reject(fn(column) -> Enum.member?(query.group_by, column) end)
  end
  defp invalid_not_aggregated_columns(%{command: :select} = query) do
    case Enum.partition(query.columns, &Function.aggregate_function?/1) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  defp compile_columns(query) do
    query
    |> expand_star_select()
    |> compile_aliases()
    |> qualify_all_identifiers()
  end

  defp expand_star_select(%{columns: :*} = query) do
    columns = for {column, _type} <- all_available_columns(query), do: column
    column_names = for {:identifier, _table, name} <- columns, do: name
    %{query | columns: columns, column_titles: column_names}
  end
  defp expand_star_select(query), do: query

  defp valid_argument_type(function_call, query) do
    case {Function.argument_type(function_call), column_with_type(function_call, query)} do
      {:any, _} -> true
      {:timestamp, {_, :timestamp}} -> true
      {:numeric, {_, :integer}} -> true
      {:numeric, {_, :real}} -> true
      _ -> false
    end
  end

  defp filter_aggregators(columns), do: Enum.filter(columns, &Function.aggregate_function?/1)

  defp verify_columns(query) do
    verify_functions(query)
    verify_aggregated_columns(query)
    verify_group_by_functions(query)
    verify_function_parameters(query)
    warn_on_selected_uids(query)
  end

  defp verify_function_parameters(query) do
    query.columns
    |> Enum.filter(&Function.function?/1)
    |> Enum.reject(&valid_argument_type(&1, query))
    |> case do
      [] -> :ok
      [function_call | _rest] ->
        {:function, function_name, _} = function_call
        function_type = Function.argument_type(function_call)
        {{:identifier, table, name}, column_type} = column_with_type(function_call, query)

        raise CompilationError, message: "Function `#{function_name}` requires `#{function_type}`,"
          <> " but used over column `#{name}` of type `#{column_type}` from table `#{table}`"
    end
  end

  defp column_with_type(select_clause, query) do
    column = select_clause_to_identifier(select_clause)
    Enum.find(all_available_columns(query), &match?({^column, _}, &1))
  end

  defp verify_aggregated_columns(query) do
    case invalid_not_aggregated_columns(query) do
      [] -> :ok
      [column | _] ->
        raise CompilationError, message: "Column `#{column_title(column)}` needs to appear in the `group by`"
          <> " clause or be used in an aggregate function."
    end
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

  defp all_available_columns(query) do
    Enum.flat_map(query.selected_tables, &columns/1)
  end

  defp select_clause_to_identifier({:function, _function, identifier}),
    do: select_clause_to_identifier(identifier)
  defp select_clause_to_identifier({:distinct, identifier}), do: identifier
  defp select_clause_to_identifier(identifier), do: identifier

  defp partition_selected_columns(%{group_by: groups = [_|_], columns: columns} = query) do
    aggregators = filter_aggregators(columns)
    Map.merge(query, %{property: groups |> Enum.uniq(), aggregators: aggregators |> Enum.uniq()})
  end
  defp partition_selected_columns(%{columns: columns} = query) do
    aggregators = filter_aggregators(columns)
    partitioned_columns = case aggregators do
      [] -> %{property: columns |> Enum.uniq(), aggregators: [{:function, "count", :*}], implicit_count: true}
      _ -> %{property: [], aggregators: aggregators |> Enum.uniq()}
    end
    Map.merge(query, partitioned_columns)
  end
  defp partition_selected_columns(query), do: query

  defp compile_order_by(%{order_by: []} = query), do: query
  defp compile_order_by(%{columns: columns, order_by: order_by_spec} = query) do
    column_table_map = construct_column_table_map(query)
    qualified_columns = Enum.map(columns, &qualify_identifier(&1, column_table_map, query))
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) ->
      Enum.member?(qualified_columns, qualify_identifier(column, column_table_map, query))
    end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          qualified_column = qualify_identifier(column, column_table_map, query)
          index = qualified_columns |> Enum.find_index(&(&1 == qualified_column))
          {index, direction}
        end
        %{query | order_by: order_list}
      [{{:identifier, table, field}, _direction} | _rest] ->
        raise CompilationError, message: "Non-selected field `#{field}` from table `#{table}` " <>
          "specified in `order by` clause."
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

    graph = :digraph.new([:private, :cyclic])
    try do
      # add uid columns as vertices
      uid_columns = qualified_uid_columns(query)
      Enum.each(uid_columns, &:digraph.add_vertex(graph, &1))

      # add edges for all `uid1 = uid2` filters
      for {:comparison, uid1, :=, uid2} <- query.where,
          uid1 != uid2,
          MapSet.member?(uid_columns, uid1),
          MapSet.member?(uid_columns, uid2)
      do
       :digraph.add_edge(graph, uid1, uid2)
       :digraph.add_edge(graph, uid2, uid1)
     end

      # Find first pair (uid1, uid2) which are not connected in the graph.
      uid_columns
      |> Stream.chunk(2, 1)
      |> Stream.filter(fn([uid1, uid2]) -> :digraph.get_path(graph, uid1, uid2) == false end)
      |> Enum.take(1)
      |> case do
            [] ->
              # No such pair -> all tables are properly joined
              query

            [[{:identifier, table1, column1}, {:identifier, table2, column2}]] ->
              raise CompilationError,
                message:
                  "Missing where comparison for uid columns of tables `#{table1}` and `#{table2}`. " <>
                  "You can fix the error by adding `#{table1}.#{column1} = #{table2}.#{column2}` " <>
                  "condition to the `WHERE` clause."
          end
    after
      # digraph is powered by ets tables, so we need to make sure they are deleted once we don't need them
      :digraph.delete(graph)
    end
  end

  defp cast_where_clauses(%{where: [_|_] = clauses} = query) do
    %{query | where: Enum.map(clauses, &cast_where_clause(&1, query))}
  end
  defp cast_where_clauses(query), do: query

  defp cast_where_clause(clause, query) do
    column = where_clause_to_identifier(clause)
    {_, type} = List.keyfind(all_available_columns(query), column, 0)
    do_cast_where_clause(clause, type)
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

  defp columns(table) do
    for {name, type} <- table.columns do
      {{:identifier, table.user_name, name}, type}
    end
  end

  defp qualify_all_identifiers(query) do
    column_table_map = construct_column_table_map(query)
    %{query |
      columns: Enum.map(query.columns, &(qualify_identifier(&1, column_table_map, query))),
      group_by: Enum.map(query.group_by, &(qualify_identifier(&1, column_table_map, query))),
      where: Enum.map(query.where, &(qualify_where_clause(&1, column_table_map, query))),
      where_not: Enum.map(query.where_not, &(qualify_where_clause(&1, column_table_map, query))),
      order_by: Enum.map(query.order_by, fn({identifier, direction}) ->
        {qualify_identifier(identifier, column_table_map, query), direction}
      end)
    }
  end

  defp construct_column_table_map(query) do
    query.selected_tables
    |> Enum.flat_map(fn(table) ->
      for {{:identifier, table, column}, _type} <- columns(table), do: {table, column}
    end)
    |> Enum.reduce(%{}, fn({table, column}, acc) ->
      Map.update(acc, column, [table], &([table | &1]))
    end)
  end

  defp qualify_identifier({:function, "count", :*} = function, _column_table_map, _query), do: function
  defp qualify_identifier({:function, function, identifier}, column_table_map, query) do
    {:function, function, qualify_identifier(identifier, column_table_map, query)}
  end
  defp qualify_identifier({:distinct, identifier}, column_table_map, query) do
    {:distinct, qualify_identifier(identifier, column_table_map, query)}
  end
  defp qualify_identifier({:identifier, :unknown, column}, column_table_map, _query) do
    case Map.get(column_table_map, column) do
      [table] -> {:identifier, table, column}
      [_|_] -> raise CompilationError, message: "Column `#{column}` is ambiguous."
      nil ->
        tables = Map.values(column_table_map)
        |> List.flatten()
        |> Enum.uniq()
        case tables do
          [table] -> raise CompilationError, message: "Column `#{column}` doesn't exist in table `#{table}`."
          [_|_] -> raise CompilationError, message: "Column `#{column}` doesn't exist in any of the selected tables."
        end
    end
  end
  defp qualify_identifier({:identifier, table, column} = identifier, column_table_map, query) do
    unless Enum.any?(query.selected_tables, &(&1.user_name == table)),
      do: raise CompilationError, message: "Missing FROM clause entry for table `#{table}`"

    case Enum.member?(Map.get(column_table_map, column, []), table) do
      true -> identifier
      false -> raise CompilationError, message: "Column `#{column}` doesn't exist in table `#{table}`."
    end
  end
  defp qualify_identifier(other, _column_table_map, _query), do: other

  defp qualify_where_clause({:comparison, lhs, comparator, rhs}, column_table_map, query) do
    {
      :comparison,
      qualify_identifier(lhs, column_table_map, query),
      comparator,
      qualify_identifier(rhs, column_table_map, query)
    }
  end
  defp qualify_where_clause({:not, subclause}, column_table_map, query) do
    {:not, qualify_where_clause(subclause, column_table_map, query)}
  end
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp qualify_where_clause({unquote(keyword), identifier, any}, column_table_map, query) do
      {unquote(keyword), qualify_identifier(identifier, column_table_map, query), any}
    end
  end)

  def column_title({:function, function, _}), do: function
  def column_title({:distinct, identifier}), do: column_title(identifier)
  def column_title({:identifier, _table, column}), do: column

  defp warn_on_selected_uids(query) do
    case selected_uid_columns(query) do
      [] -> query
      [_|_] = selected_uid_columns ->
        Enum.reduce(selected_uid_columns, query, &warn_on_selected_uid(&2, &1))
    end
  end

  defp selected_uid_columns(query) do
    all_uid_columns = qualified_uid_columns(query)
    Enum.filter(query.columns, &MapSet.member?(all_uid_columns, &1))
  end

  defp qualified_uid_columns(query) do
    query.selected_tables
    |> Enum.map(&{:identifier, &1.user_name, &1.user_id})
    |> MapSet.new()
  end

  defp warn_on_selected_uid(query, {:identifier, table_name, column_name}) do
    add_info_message(query,
      "Selecting column `#{column_name}` from table `#{table_name}` will cause all values to be anonymized. " <>
      "Consider removing this column from the list of selected columns."
    )
  end

  defp add_info_message(query, info_message), do: %{query | info: [info_message | query.info]}
end
