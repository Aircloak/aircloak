defmodule Cloak.SqlQuery.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token

  @type compiled_query :: %{
    data_source: atom,
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


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(atom, Parser.parsed_query) :: {:ok, compiled_query} | {:error, String.t}
  def compile(data_source, query) do
    defaults = %{data_source: data_source, where_not: [], unsafe_filter_columns: [], group_by: []}
    query = Map.merge(defaults, query)
    with {:ok, query} <- compile_from(query),
         {:ok, query} <- compile_columns(query),
         {:ok, query} <- compile_order_by(query),
         {:ok, query} <- cast_where_clauses(query),
         query = partition_selected_columns(query),
         query = partition_where_clauses(query),
      do: {:ok, query}
  end

  @doc "Returns a string title for the given column specification."
  @spec column_title(Parser.column) :: String.t
  def column_title({:function, function, identifier}), do: "#{function}(#{column_title(identifier)})"
  def column_title({:distinct, identifier}), do: "distinct #{identifier}"
  def column_title(column), do: column


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_from(%{from: {:subquery, _}} = query) do
    # We currently treat subqueries as absolute blackboxes, and therefore just have to trust the analyst
    # to having selected a table that actually exists. This is unfortunate, but can't be improved
    # until we do full subquery parsing ourselves.
    {:ok, query}
  end
  defp compile_from(%{from: table_identifier, data_source: data_source} = query) do
    tables = DataSource.tables(data_source)
    case Enum.find_index(tables, &(Atom.to_string(&1) === table_identifier)) do
      nil -> {:error, ~s/Table `#{table_identifier}` doesn't exist./}
      _ -> {:ok, query}
    end
  end
  defp compile_from(query), do: {:ok, query}

  defp invalid_not_aggregated_columns(%{command: :select, group_by: [_|_]} = query) do
    Enum.reject(query.columns, &(aggregate_function?(&1) || Enum.member?(query.group_by, &1)))
  end
  defp invalid_not_aggregated_columns(%{command: :select} = query) do
    case Enum.partition(query.columns, &aggregate_function?/1) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  @aggregation_functions ~w(count sum avg min max stddev median)
  defp aggregate_function?({:function, function, _}), do: Enum.member?(@aggregation_functions, function)
  defp aggregate_function?(_), do: false

  @numeric_aggregate_functions ~w(sum avg min max stddev median)
  defp numeric_aggregate_function?({:function, function, _}),
    do: Enum.member?(@numeric_aggregate_functions, function)

  defp filter_aggregators(columns), do: Enum.filter(columns, &aggregate_function?/1)

  defp compile_columns(%{command: :select, from: {:subquery, _}} = query) do
    # Subqueries can produce column-names that are not actually in the table. Without understanding what
    # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
    # the outer column selections
    {:ok, query}
  end
  defp compile_columns(%{command: :select, columns: :*, from: table_identifier, data_source: data_source} = query) do
    columns = for {name, _type} <- columns(table_identifier, data_source), do: name
    compile_columns(%{query | columns: columns})
  end
  defp compile_columns(%{command: :select, from: table_identifier, data_source: data_source} = query) do
    with table_columns = columns(table_identifier, data_source),
        :ok <- verify_column_names(query, table_columns),
        :ok <- verify_aggregated_columns(query),
        :ok <- verify_functions(query),
        :ok <- verify_function_parameters(query, table_columns),
      do: {:ok, query}
  end
  defp compile_columns(query), do: {:ok, query}

  defp verify_function_parameters(query, table_columns) do
    aggregated_columns = for {:function, _, _} = column <- query.columns,
      numeric_aggregate_function?(column), do: select_clause_to_identifier(column)
    invalid_columns = Enum.reject(aggregated_columns,
      &(Enum.member?(table_columns, {&1, :integer}) or Enum.member?(table_columns, {&1, :real})))
    case invalid_columns do
      [] -> :ok
      [invalid_column | _rest] ->
        {:error, ~s/Aggregation function used over non-numeric column `#{invalid_column}`./}
    end
  end

  defp verify_aggregated_columns(query) do
    case invalid_not_aggregated_columns(query) do
      [] -> :ok
      [invalid_column | _rest] ->
        {
          :error,
          "Column `#{invalid_column}` needs to appear in the " <>
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

  defp verify_column_names(query, table_columns) do
    valid_names = [:* | (for {name, _type} <- table_columns, do: name)]
    invalid_columns = Enum.reject(all_columns(query), &Enum.member?(valid_names, &1))
    case invalid_columns do
      [] -> :ok
      [invalid_column | _rest] -> {:error, ~s/Column `#{invalid_column}` doesn't exist./}
    end
  end

  defp all_columns(query) do
    select_columns = for column <- query.columns, do: select_clause_to_identifier(column)
    where_columns = for column <- Map.get(query, :where, []), do: where_clause_to_identifier(column)
    select_columns ++ where_columns ++ query.group_by
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

  defp compile_order_by(%{columns: columns, order_by: order_by_spec} = query) do
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) -> Enum.member?(columns, column) end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          {column, direction}
        end
        {:ok, %{query | order_by: order_list}}
      [{invalid_field, _direction} | _rest] ->
        {:error, ~s/Non-selected field `#{column_title(invalid_field)}` specified in `order by` clause./}
    end
  end
  defp compile_order_by(query), do: {:ok, query}

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

  defp cast_where_clause(clause, %{from: {:subquery, _}}) do
    do_cast_where_clause(clause, :unknown)
  end
  defp cast_where_clause(clause, query) do
    column = where_clause_to_identifier(clause)
    {_, type} = columns(query.from, query.data_source)
      |> List.keyfind(column, 0)

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
    DataSource.columns(data_source, table_id)
  end

  defp error_map(xs, fun) do
    result = Enum.map(xs, fun)

    case Enum.find(result, &match?({:error, _}, &1)) do
      {:error, error} -> {:error, error}
      _ -> {:ok, Enum.map(result, fn({:ok, x}) -> x end)}
    end
  end
end
