defmodule Cloak.SqlQuery.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."
  alias Cloak.DataSource

  @type compiled_query :: %{
    data_source: atom,
    command: :select | :show,
    columns: [Parser.column],
    group_by: [String.t],
    from: [String.t],
    where: [Parser.where_clause],
    order_by: [{pos_integer, :asc | :desc}],
    show: :tables | :columns
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(atom, Parser.parsed_query) :: {:ok, compiled_query} | {:error, String.t}
  def compile(data_source, query) do
    query = Map.put(query, :data_source, data_source)
    with {:ok, query} <- compile_from(query),
         {:ok, query} <- compile_columns(query),
         {:ok, query} <- compile_order_by(query),
      do: {:ok, query}
  end

  @doc "Returns a string title for the given column specification."
  @spec column_title(Parser.column) :: String.t
  def column_title({:aggregate, "count", :star}), do: "count(*)"
  def column_title({:aggregate, function, identifier}), do: "#{function}(#{identifier})"
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

  defp aggregate_function?({:aggregate, _, _}), do: true
  defp aggregate_function?(_), do: false

  defp compile_columns(%{command: :select, from: {:subquery, _}} = query) do
    # Subqueries can produce column-names that are not actually in the table. Without understanding what
    # is being produced by the subquery (currently it is being treated as a blackbox), we cannot validate
    # the outer column selections
    {:ok, query}
  end
  defp compile_columns(%{command: :select, columns: :star, from: table_identifier, data_source: data_source} = query) do
    table_id = String.to_existing_atom(table_identifier)
    columns = for {name, _type} <- DataSource.columns(data_source, table_id), do: name
    compile_columns(%{query | columns: columns})
  end
  defp compile_columns(%{command: :select, from: table_identifier, data_source: data_source} = query) do
    table_id = String.to_existing_atom(table_identifier)
    table_columns = DataSource.columns(data_source, table_id)
    with :ok <- verify_column_names(query, table_columns),
        :ok <- verify_aggregated_columns(query),
        :ok <- verify_aggregation_functions(query),
        :ok <- verify_aggregated_types(query, table_columns),
      do: {:ok, query}
  end
  defp compile_columns(query), do: {:ok, query}

  defp verify_aggregated_types(query, table_columns) do
    aggregated_columns = for {:aggregate, function, identifier} <- query.columns, function !== "count", do: identifier
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

  defp verify_aggregation_functions(query) do
    functions = for {:aggregate, function, _} <- query.columns, do: function
    valid_functions = ["count", "sum", "avg", "min", "max", "stddev", "median"]
    invalid_functions = Enum.reject(functions, &(Enum.member?(valid_functions, &1)))
    case invalid_functions do
      [] -> :ok
      [invalid_function | _rest] ->
        {:error, ~s/Unknown aggregation function `#{invalid_function}`./}
    end
  end

  defp verify_column_names(query, table_columns) do
    names = for {name, _type} <- table_columns, do: name
    invalid_columns = Enum.reject(all_columns(query), &Enum.member?(names, &1))
    case invalid_columns do
      [] -> :ok
      [invalid_column | _rest] -> {:error, ~s/Column `#{invalid_column}` doesn't exist./}
    end
  end

  defp all_columns(query) do
    select_columns = for column <- query.columns, do: select_clause_to_identifier(column)
    where_columns = for column <- Map.get(query, :where, []), do: where_clause_to_identifier(column)
    group_by_columns = Map.get(query, :group_by, [])
    (select_columns -- [:star]) ++ where_columns ++ group_by_columns
  end

  defp select_clause_to_identifier({:aggregate, _function, identifier}), do: identifier
  defp select_clause_to_identifier(identifier), do: identifier

  defp where_clause_to_identifier({:comparison, identifier, _, _}), do: identifier
  defp where_clause_to_identifier({:not, subclause}), do: where_clause_to_identifier(subclause)
  Enum.each([:in, :like, :ilike, :is], fn(keyword) ->
    defp where_clause_to_identifier({unquote(keyword), identifier, _}), do: identifier
  end)

  defp compile_order_by(%{columns: columns, order_by: order_by_spec} = query) do
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) -> Enum.member?(columns, column) end)
    case invalid_fields do
      [] ->
        order_list = for {column, direction} <- order_by_spec do
          index = columns |> Enum.find_index(&(&1 == column))
          {index, direction}
        end
        {:ok, %{query | order_by: order_list}}
      [{invalid_field, _direction} | _rest] ->
        {:error, ~s/Non-selected field `#{column_title(invalid_field)}` specified in `order by` clause./}
    end
  end
  defp compile_order_by(query), do: {:ok, query}
end
