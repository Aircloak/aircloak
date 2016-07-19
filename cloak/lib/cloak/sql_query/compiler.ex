defmodule Cloak.SqlQuery.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token

  @type compiled_query :: %{
    data_source: DataSource.t,
    command: :select | :show,
    columns: [Parser.column],
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

  defmodule AmbiguousIdentifier do
    defexception message: "Ambiguous identifier"
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(atom, Parser.parsed_query) :: {:ok, compiled_query} | {:error, String.t}
  def compile(data_source, query) do
    defaults = %{data_source: data_source, where: [], where_not: [], unsafe_filter_columns: [],
      group_by: [], order_by: []}
    compile_prepped_query(Map.merge(defaults, query))
  end

  @doc "Returns a string title for the given column specification."
  @spec column_title(Parser.column) :: String.t
  def column_title({:function, function, identifier}), do: "#{function}(#{column_title(identifier)})"
  def column_title({:distinct, identifier}), do: "distinct #{column_title(identifier)}"
  def column_title({:qualified, table, column}), do: "#{table}.#{column_title(column)}"
  def column_title(column), do: column


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
    with :ok <- ds_proxy_validate_no_wildcard(query),
         :ok <- ds_proxy_validate_no_where(query),
         :ok <- verify_aggregated_columns(query),
         {:ok, query} <- compile_order_by(query),
         query = partition_selected_columns(query),
      do: {:ok, query}
  end
  defp compile_prepped_query(%{command: :show} = query), do: compile_from(query)
  defp compile_prepped_query(query) do
    with {:ok, query} <- compile_from(query),
         query = expand_star_select(query),
         :ok <- validate_all_requested_tables_are_selected(query),
         {:ok, query} <- qualify_all_identifiers(query),
         {:ok, query} <- compile_columns(query),
         {:ok, query} <- compile_order_by(query),
         {:ok, query} <- cast_where_clauses(query),
         query = partition_selected_columns(query),
         query = partition_where_clauses(query),
      do: {:ok, query}
  end


  # -------------------------------------------------------------------
  # DS Proxy validators.
  # -------------------------------------------------------------------

  defp ds_proxy_validate_no_wildcard(%{command: :select, columns: :*}) do
    {:error, "Unfortunately wildcard selects are not supported together with subselects"}
  end
  defp ds_proxy_validate_no_wildcard(_), do: :ok

  defp ds_proxy_validate_no_where(%{where: []}), do: :ok
  defp ds_proxy_validate_no_where(_) do
    {:error, "WHERE-clause in outer SELECT is not allowed in combination with a subquery"}
  end


  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp compile_from(%{from: from_clause, data_source: data_source} = query) do
    tables = Enum.map(DataSource.tables(data_source), &Atom.to_string/1)
    selected_tables = from_clause_to_tables(from_clause)
    case selected_tables -- tables do
      [] -> {:ok, query}
      [table | _] -> {:error, ~s/Table `#{table}` doesn't exist./}
    end
  end
  defp compile_from(query), do: {:ok, query}

  defp from_clause_to_tables({:cross_join, table, rest}), do: [table | from_clause_to_tables(rest)]
  defp from_clause_to_tables(table), do: [table]

  defp invalid_not_aggregated_columns(%{command: :select, group_by: [_|_]} = query) do
    Enum.reject(query.columns, &(aggregate_function?(&1) || Enum.member?(query.group_by, &1)))
  end
  defp invalid_not_aggregated_columns(%{command: :select} = query) do
    case Enum.partition(query.columns, &aggregate_function?/1) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  defp expand_star_select(%{columns: :*} = query) do
    columns = for {column, _type} <- all_available_columns(query), do: column
    %{query | columns: columns}
  end
  defp expand_star_select(query), do: query

  defp validate_all_requested_tables_are_selected(%{from: from_clause} = query) do
    all_identifiers = query.columns ++ Map.get(query, :group_by, []) ++
      Map.get(query, :where, []) ++ Map.get(query, :order_by, [])
    referenced_tables = all_identifiers
    |> Enum.map(&extract_identifier/1)
    |> Enum.reject(&(&1 == :*))
    |> Enum.map(fn
      ({:qualified, table, _}) -> table
      (_) -> :drop
    end)
    |> Enum.reject(&(&1 == :drop))
    |> Enum.uniq()
    case referenced_tables -- from_clause_to_tables(from_clause) do
      [] -> :ok
      [table | _] -> {:error, ~s/Missing FROM clause entry for table `#{table}`/}
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

  @aggregate_functions ~w(count sum avg min max stddev median)
  defp aggregate_function?({:function, function, _}), do: Enum.member?(@aggregate_functions, function)
  defp aggregate_function?(_), do: false

  @numeric_aggregate_functions ~w(sum avg min max stddev median)
  defp numeric_aggregate_function?({:function, function, _}),
    do: Enum.member?(@numeric_aggregate_functions, function)

  defp filter_aggregators(columns), do: Enum.filter(columns, &aggregate_function?/1)

  defp compile_columns(query) do
    available_columns = all_available_columns(query)
    with :ok <- verify_aggregated_columns(query),
         :ok <- verify_functions(query),
         :ok <- verify_function_parameters(query, available_columns),
      do: {:ok, query}
  end

  defp verify_function_parameters(query, available_columns) do
    aggregated_columns = for {:function, _, _} = column <- query.columns,
      numeric_aggregate_function?(column), do: select_clause_to_identifier(column)
    invalid_columns = Enum.reject(aggregated_columns,
      &(Enum.member?(available_columns, {&1, :integer}) or Enum.member?(available_columns, {&1, :real})))
    case invalid_columns do
      [] -> :ok
      [{:qualified, table, invalid_column} | _rest] ->
        {:error, ~s/Aggregation function used over non-numeric column `#{invalid_column}` from table `#{table}`./}
    end
  end

  defp verify_aggregated_columns(query) do
    case invalid_not_aggregated_columns(query) do
      [] -> :ok
      [{:qualified, table, invalid_column} | _rest] ->
        {
          :error,
          "Column `#{invalid_column}` from table `#{table}` needs to appear in the " <>
            "`group by` clause or be used in an aggregate function."
        }
    end
  end

  defp verify_functions(query) do
    invalid_functions = for {:function, function, _} = column <- query.columns,
      !aggregate_function?(column), do: function
    case invalid_functions do
      [] -> :ok
      [invalid_function | _rest] ->
        {:error, ~s/Unknown function `#{invalid_function}`./}
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

  defp compile_order_by(%{order_by: []} = query), do: {:ok, query}
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
        {:ok, %{query | order_by: order_list}}
      [{{:qualified, table, field}, _direction} | _rest] ->
        {:error, ~s/Non-selected field `#{field}` from table `#{table}` specified in `order by` clause./}
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
    case error_map(clauses, &cast_where_clause(&1, query)) do
      {:ok, result} -> {:ok, %{query | where: result}}
      error -> error
    end
  end
  defp cast_where_clauses(query), do: {:ok, query}

  defp cast_where_clause(clause, query) do
    column = where_clause_to_identifier(clause)
    {_, type} = List.keyfind(all_available_columns(query), column, 0)
    do_cast_where_clause(clause, type)
  end

  defp do_cast_where_clause({:not, subclause}, type) do
    case do_cast_where_clause(subclause, type) do
      {:ok, result} -> {:ok, {:not, result}}
      error -> error
    end
  end
  defp do_cast_where_clause({:comparison, identifier, comparator, rhs}, :timestamp) do
    case parse_time(rhs) do
      {:ok, time} -> {:ok, {:comparison, identifier, comparator, time}}
      error -> error
    end
  end
  defp do_cast_where_clause({:in, column, values}, :timestamp) do
    case error_map(values, &parse_time/1) do
      {:ok, times} -> {:ok, {:in, column, times}}
      error -> error
    end
  end
  defp do_cast_where_clause(clause, _), do: {:ok, clause}

  defp parse_time(%Token{category: :constant, value: %{type: :string, value: string}}) do
    case Timex.parse(string, "{ISO}") do
      {:ok, value} -> {:ok, value}
      _ -> case Timex.parse(string, "{ISOdate}") do
        {:ok, value} -> {:ok, value}
        _ -> {:error, "Cannot cast `#{string}` to timestamp."}
      end
    end
  end
  defp parse_time(%Token{value: %{value: value}}), do: {:error, "Cannot cast `#{value}` to timestamp."}

  defp where_clause_to_identifier({:comparison, identifier, _, _}), do: identifier
  defp where_clause_to_identifier({:not, subclause}), do: where_clause_to_identifier(subclause)
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp where_clause_to_identifier({unquote(keyword), identifier, _}), do: identifier
  end)

  defp columns(table, data_source) do
    table_id = String.to_existing_atom(table)
    for {name, type} <- DataSource.columns(data_source, table_id) do
      {{:qualified, Atom.to_string(table_id), name}, type}
    end
  end

  defp error_map(xs, fun) do
    result = Enum.map(xs, fun)

    case Enum.find(result, &match?({:error, _}, &1)) do
      {:error, error} -> {:error, error}
      _ -> {:ok, Enum.map(result, fn({:ok, x}) -> x end)}
    end
  end

  defp qualify_all_identifiers(query) do
    column_table_map = construct_column_table_map(query)
    try do
      query = %{query |
        columns: Enum.map(query.columns, &(qualify_identifier(&1, column_table_map))),
        group_by: Enum.map(query.group_by, &(qualify_identifier(&1, column_table_map))),
        where: Enum.map(query.where, &(qualify_where_clause(&1, column_table_map))),
        where_not: Enum.map(query.where_not, &(qualify_where_clause(&1, column_table_map))),
        order_by: Enum.map(query.order_by, fn({identifier, direction}) ->
          {qualify_identifier(identifier, column_table_map), direction}
        end)
      }
      {:ok, query}
    rescue
      e in AmbiguousIdentifier ->
        {:error, e.message}
    end
  end

  defp construct_column_table_map(%{from: from_clause, data_source: data_source}) do
    from_clause_to_tables(from_clause)
    |> Enum.flat_map(fn(table) ->
      for {{:qualified, table, column}, _type} <- columns(table, data_source), do: {table, column}
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
  defp qualify_identifier({:qualified, table, column} = identifier, column_table_map) do
    case [table] -- Map.get(column_table_map, column, []) do
      [] -> identifier
      _ -> raise AmbiguousIdentifier, message: "Column `#{column}` doesn't exist in table `#{table}`."
    end
  end
  defp qualify_identifier(identifier, column_table_map) do
    case Map.get(column_table_map, identifier) do
      [table] -> {:qualified, table, identifier}
      [_|_] -> raise AmbiguousIdentifier, message: "Column `#{identifier}` is ambiguous."
      nil ->
        tables = Map.values(column_table_map)
        |> List.flatten()
        |> Enum.uniq()
        case tables do
          [table] ->
            raise AmbiguousIdentifier, message: "Column `#{identifier}` doesn't exist in table `#{table}`."
          [_|_] ->
            raise AmbiguousIdentifier,
              message: "Column `#{identifier}` doesn't exist in any of the selected tables."
        end
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
end
