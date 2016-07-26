defmodule Cloak.SqlQuery.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token

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
    show: :tables | :columns
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
      group_by: [], order_by: [], column_titles: []}
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
      {:ok, compile_from(query)}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end
  defp compile_prepped_query(query) do
    try do
      query = query
      |> compile_from()
      |> expand_star_select()
      |> compile_aliases()
      |> validate_all_requested_tables_are_selected()
      |> qualify_all_identifiers()
      |> compile_columns()
      |> compile_order_by()
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

  defp compile_from(%{from: from_clause, data_source: data_source} = query) do
    tables = Enum.map(DataSource.tables(data_source), &Atom.to_string/1)
    selected_tables = from_clause_to_tables(from_clause)
    case selected_tables -- tables do
      [] -> query
      [table | _] -> raise CompilationError, message: "Table `#{table}` doesn't exist."
    end
  end
  defp compile_from(query), do: query

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
    Enum.reject(query.columns, fn(column) ->
      aggregate_function?(column) || Enum.member?(query.group_by, select_clause_to_identifier(column))
    end)
  end
  defp invalid_not_aggregated_columns(%{command: :select} = query) do
    case Enum.partition(query.columns, &aggregate_function?/1) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  defp expand_star_select(%{columns: :*} = query) do
    columns = for {column, _type} <- all_available_columns(query), do: column
    column_names = for {:identifier, _table, name} <- columns, do: name
    %{query | columns: columns, column_titles: column_names}
  end
  defp expand_star_select(query), do: query

  defp validate_all_requested_tables_are_selected(%{from: from_clause} = query) do
    all_identifiers = query.columns ++ Map.get(query, :group_by, []) ++
      Map.get(query, :where, []) ++ Map.get(query, :order_by, [])
    referenced_tables = all_identifiers
    |> Enum.map(&extract_identifier/1)
    |> Enum.reject(&(&1 == :*))
    |> Enum.map(fn({:identifier, table, _}) -> table end)
    |> Enum.reject(&(&1 == :unknown))
    |> Enum.uniq()
    case referenced_tables -- from_clause_to_tables(from_clause) do
      [] -> query
      [table | _] -> raise CompilationError, message: ~s/Missing FROM clause entry for table `#{table}`/
    end
  end

  defp extract_identifier({:function, _function, identifier}), do: extract_identifier(identifier)
  defp extract_identifier({:distinct, identifier}), do: extract_identifier(identifier)
  defp extract_identifier({:comparison, identifier, _comparator, _any}), do: extract_identifier(identifier)
  defp extract_identifier({:not, subclause}), do: extract_identifier(subclause)
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp extract_identifier({unquote(keyword), identifier, _}), do: extract_identifier(identifier)
  end)
  Enum.each([:asc, :desc, :nil], fn(keyword) ->
    defp extract_identifier({identifier, unquote(keyword)}), do: extract_identifier(identifier)
  end)
  defp extract_identifier(entry), do: entry

  @functions %{
    ~w(count) => %{aggregate: true, type: :any},
    ~w(sum avg min max stddev median) => %{aggregate: true, type: :numeric},
    ~w(year month day hour minute second weekday) => %{aggregate: false, type: :timestamp},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  defp function?({:function, _, _}), do: true
  defp function?(_), do: false

  defp valid_function?({:function, function, _}), do: Map.has_key?(@functions, function)

  defp aggregate_function?({:function, function, _}), do: @functions[function].aggregate
  defp aggregate_function?(_), do: false

  defp parameter_type({:function, function, _}), do: @functions[function].type

  defp valid_parameter_type?(function_call, query) do
    case {parameter_type(function_call), column_with_type(function_call, query)} do
      {:any, _} -> true
      {:timestamp, {_, :timestamp}} -> true
      {:numeric, {_, :integer}} -> true
      {:numeric, {_, :real}} -> true
      _ -> false
    end
  end

  defp filter_aggregators(columns), do: Enum.filter(columns, &aggregate_function?/1)

  defp compile_columns(query) do
    verify_functions(query)
    verify_aggregated_columns(query)
    verify_function_parameters(query)
    query
  end

  defp verify_function_parameters(query) do
    query.columns
    |> Enum.filter(&function?/1)
    |> Enum.reject(&valid_parameter_type?(&1, query))
    |> case do
      [] -> :ok
      [function_call | _rest] ->
        {:function, function_name, _} = function_call
        function_type = parameter_type(function_call)
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
      [{:identifier, table, invalid_column} | _rest] ->
        raise CompilationError, message: "Column `#{invalid_column}` from table `#{table}` needs " <>
          "to appear in the `group by` clause or be used in an aggregate function."
    end
  end

  defp verify_functions(query) do
    query.columns
    |> Enum.filter(&function?/1)
    |> Enum.reject(&valid_function?/1)
    |> case do
      [] -> :ok
      [{:function, invalid_function, _} | _rest] ->
        raise CompilationError, message: ~s/Unknown function `#{invalid_function}`./
    end
  end

  defp all_available_columns(%{from: from_clause, data_source: data_source}) do
    Enum.flat_map(from_clause_to_tables(from_clause), &(columns(&1, data_source)))
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
    qualified_columns = Enum.map(columns, &qualify_identifier(&1, column_table_map))
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) ->
      Enum.member?(qualified_columns, qualify_identifier(column, column_table_map))
    end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          qualified_column = qualify_identifier(column, column_table_map)
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

  defp columns(table, data_source) do
    table_id = String.to_existing_atom(table)
    for {name, type} <- DataSource.columns(data_source, table_id) do
      {{:identifier, Atom.to_string(table_id), name}, type}
    end
  end

  defp qualify_all_identifiers(query) do
    column_table_map = construct_column_table_map(query)
    %{query |
      columns: Enum.map(query.columns, &(qualify_identifier(&1, column_table_map))),
      group_by: Enum.map(query.group_by, &(qualify_identifier(&1, column_table_map))),
      where: Enum.map(query.where, &(qualify_where_clause(&1, column_table_map))),
      where_not: Enum.map(query.where_not, &(qualify_where_clause(&1, column_table_map))),
      order_by: Enum.map(query.order_by, fn({identifier, direction}) ->
        {qualify_identifier(identifier, column_table_map), direction}
      end)
    }
  end

  defp construct_column_table_map(%{from: from_clause, data_source: data_source}) do
    from_clause_to_tables(from_clause)
    |> Enum.flat_map(fn(table) ->
      for {{:identifier, table, column}, _type} <- columns(table, data_source), do: {table, column}
    end)
    |> Enum.reduce(%{}, fn({table, column}, acc) ->
      Map.update(acc, column, [table], &([table | &1]))
    end)
  end

  defp qualify_identifier({:function, "count", :*} = function, _column_table_map), do: function
  defp qualify_identifier({:function, function, identifier}, column_table_map) do
    {:function, function, qualify_identifier(identifier, column_table_map)}
  end
  defp qualify_identifier({:distinct, identifier}, column_table_map) do
    {:distinct, qualify_identifier(identifier, column_table_map)}
  end
  defp qualify_identifier({:identifier, :unknown, column}, column_table_map) do
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
  defp qualify_identifier({:identifier, table, column} = identifier, column_table_map) do
    case Enum.member?(Map.get(column_table_map, column, []), table) do
      true -> identifier
      false -> raise CompilationError, message: "Column `#{column}` doesn't exist in table `#{table}`."
    end
  end

  defp qualify_where_clause({:comparison, identifier, comparator, any}, column_table_map) do
    {:comparison, qualify_identifier(identifier, column_table_map), comparator, any}
  end
  defp qualify_where_clause({:not, subclause}, column_table_map) do
    {:not, qualify_where_clause(subclause, column_table_map)}
  end
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp qualify_where_clause({unquote(keyword), identifier, any}, column_table_map) do
      {unquote(keyword), qualify_identifier(identifier, column_table_map), any}
    end
  end)

  def column_title({:function, function, _}), do: function
  def column_title({:distinct, identifier}), do: column_title(identifier)
  def column_title({:identifier, _table, column}), do: column
end
