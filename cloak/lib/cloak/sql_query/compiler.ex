defmodule Cloak.SqlQuery.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."
  alias Cloak.DataSource

  @type compiled_query :: %{
    data_source: atom,
    command: :select | :show,
    columns: [Parser.column],
    filter_columns: [Parser.column],
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
    query = Map.merge(query, %{data_source: data_source, where_not: [], filter_columns: []})
    with {:ok, query} <- compile_from(query),
         {:ok, query} <- compile_columns(query),
         {:ok, query} <- compile_aggregation(query),
         {:ok, query} <- compile_order_by(query),
         {:ok, query} <- compile_where_not(query),
      do: {:ok, query}
  end

  @doc "Returns a string title for the given column specification."
  @spec column_title(Parser.column) :: String.t
  def column_title({:count, :star}), do: "count(*)"
  def column_title(column), do: column


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_from(%{from: table_identifier, data_source: data_source} = query) do
    tables = DataSource.tables(data_source)
    case Enum.find_index(tables, &(Atom.to_string(&1) === table_identifier)) do
      nil -> {:error, ~s/Table "#{table_identifier}" doesn't exist./}
      _ -> {:ok, query}
    end
  end
  defp compile_from(query), do: {:ok, query}

  defp compile_aggregation(%{command: :select} = query) do
    case invalid_not_aggregated_columns(query) do
      [] -> {:ok, query}
      columns ->
        {
          :error,
          "Columns #{columns |> Enum.map(&column_title/1) |> Enum.join} need to appear in the " <>
            "`group by` clause or be used in an aggregate function."
        }
    end
  end
  defp compile_aggregation(query), do: {:ok, query}

  defp invalid_not_aggregated_columns(%{command: :select, group_by: [_|_]} = query) do
    Enum.reject(query.columns, &(aggregate_function?(&1) || Enum.member?(query.group_by, &1)))
  end
  defp invalid_not_aggregated_columns(%{command: :select} = query) do
    case Enum.partition(query.columns, &aggregate_function?/1) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  defp aggregate_function?({:count, _}), do: true
  defp aggregate_function?(_), do: false

  defp compile_columns(%{command: :select, columns: :star, from: table_identifier, data_source: data_source} = query) do
    table_id = String.to_existing_atom(table_identifier)
    columns = for {name, _type} <- DataSource.columns(data_source, table_id), do: name
    {:ok, %{query | columns: columns}}
  end
  defp compile_columns(%{command: :select, from: table_identifier, data_source: data_source} = query) do
    table_id = String.to_existing_atom(table_identifier)
    columns = for {name, _type} <- DataSource.columns(data_source, table_id), do: name
    invalid_columns = Enum.reject(all_columns(query), &valid_column?(&1, columns))
    case invalid_columns do
      [] -> {:ok, query}
      [invalid_column | _rest] -> {:error, ~s/Column "#{invalid_column}" doesn't exist./}
    end
  end
  defp compile_columns(query), do: {:ok, query}

  defp valid_column?({:count, :star}, _), do: true
  defp valid_column?(name, columns) do
    columns |> Enum.any?(&(&1 == name))
  end

  defp all_columns(%{columns: selected_columns} = query) do
    where_columns = Map.get(query, :where, [])
    |> Enum.map(&where_clause_to_identifier/1)

    group_by_columns = Map.get(query, :group_by, [])

    selected_columns ++ where_columns ++ group_by_columns
  end

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
        {:error, ~s/Non-selected field specified in 'order by' clause: #{inspect invalid_field}./}
    end
  end
  defp compile_order_by(query), do: {:ok, query}

  defp compile_where_not(%{where: clauses} = query) do
    {negative, positive} = Enum.partition(clauses, fn(clause) -> match?({:not, _}, clause) end)
    negative = Enum.map(negative, fn({:not, clause}) -> clause end)
    filter_columns = Enum.map(negative, &where_clause_to_identifier/1)

    {:ok, %{query | where: positive, where_not: negative, filter_columns: filter_columns}}
  end
  defp compile_where_not(query), do: {:ok, query}

  defp where_clause_to_identifier({:comparison, identifier, _, _}), do: identifier
  defp where_clause_to_identifier({:not, subclause}), do: where_clause_to_identifier(subclause)
  Enum.each([:in, :like, :ilike], fn(keyword) ->
    defp where_clause_to_identifier({unquote(keyword), identifier, _}), do: identifier
  end)
end
