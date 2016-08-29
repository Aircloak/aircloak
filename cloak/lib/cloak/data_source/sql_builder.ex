defmodule Cloak.DataSource.SqlBuilder do
  @moduledoc "Provides functionality for constructing an SQL query from a compiled query."

  alias Cloak.Aql.Query
  alias Cloak.Aql.Column


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @spec build(Query.t) :: String.t
  @doc "Constructs a parametrized SQL query that can be executed against a backend"
  def build(%Query{mode: :unparsed} = query) do
    {:subquery, %{unparsed_string: unsafe_subquery}} = query.from
    to_string(["SELECT ", columns_sql(query.db_columns), " FROM (", unsafe_subquery, ") AS unsafe_subquery"])
  end
  def build(query) do
    query
    |> build_fragments()
    |> to_string()
  end

  @doc "Returns a name uniquely identifying a column in the generated query."
  @spec column_name(Column.t) :: String.t
  def column_name(%Column{table: :unknown, name: name}), do: "\"#{name}\""
  def column_name(column), do: "\"#{column.table.name}\".\"#{column.name}\""


  # -------------------------------------------------------------------
  # Transformation of query AST to query specification
  # -------------------------------------------------------------------

  defp build_fragments(query) do
    [
      "SELECT ", columns_sql(query.db_columns), " ",
      "FROM ", from_clause(query.from, query), " ",
      where_fragments(query.where),
      group_by_fragments(query)
    ]
  end

  defp columns_sql(columns) do
    columns
    |> Enum.map(&column_sql/1)
    |> Enum.intersperse(?,)
  end

  defp column_sql({:distinct, column}),
    do: ["DISTINCT ", column_sql(column)]
  defp column_sql(%Column{alias: alias} = column) when alias != nil do
    [column_sql(%Column{column | alias: nil}), "AS ", alias]
  end
  defp column_sql(%Column{db_function: fun_name, db_function_args: args, type: type}) when fun_name != nil,
    do: function_sql(fun_name, args, type)
  defp column_sql(%Column{constant?: true, value: value, type: type}),
    do: [cast(constant_to_fragment(value), sql_type(type))]
  defp column_sql(column), do: column_name(column)

  # aggregate functions
  defp function_sql("min", [arg], _type), do: function_call("min", [column_sql(arg)])
  defp function_sql("max", [arg], _type), do: function_call("max", [column_sql(arg)])
  defp function_sql("avg", [arg], type), do: cast(function_call("avg", [column_sql(arg)]), sql_type(type))
  defp function_sql("sum", [arg], type), do: cast(function_call("sum", [column_sql(arg)]), sql_type(type))
  defp function_sql("count", [:*], _type), do: cast(function_call("count", ["*"]), "integer")
  defp function_sql("count", [arg], _type), do: cast(function_call("count", [column_sql(arg)]), "integer")
  # math functions
  defp function_sql("trunc", [arg], _type), do: cast(function_call("trunc", [column_sql(arg)]), "integer")
  defp function_sql("trunc", [arg1, arg2], _type),
    do: cast(function_call("trunc", [cast(column_sql(arg1), "numeric"), column_sql(arg2)]), "float")
  defp function_sql("round", [arg], _type), do: cast(function_call("round", [column_sql(arg)]), "integer")
  defp function_sql("round", [arg1, arg2], _type),
    do: cast(function_call("round", [cast(column_sql(arg1), "numeric"), column_sql(arg2)]), "float")
  defp function_sql("abs", [arg], _type), do: function_call("abs", [column_sql(arg)])
  defp function_sql("sqrt", [arg], _type), do: function_call("sqrt", [column_sql(arg)])
  defp function_sql("div", [arg1, arg2], _type),
    do: cast(function_call("div", [column_sql(arg1), column_sql(arg2)]), "integer")
  defp function_sql("mod", [arg1, arg2], _type),
    do: cast(function_call("mod", [column_sql(arg1), column_sql(arg2)]), "integer")
  defp function_sql("pow", [arg1, arg2], _type), do: function_call("pow", [column_sql(arg1), column_sql(arg2)])
  # datetime functions
  for datepart <- ["year", "month", "day", "hour", "minute", "second"] do
    defp function_sql(unquote(datepart), [arg], _type) do
      function_call("extract", [[unquote(datepart), " FROM ", column_sql(arg)]])
      |> cast("integer")
    end
  end
  # string functions
  defp function_sql("length", [arg], _type), do: function_call("length", [column_sql(arg)])
  defp function_sql("lower", [arg], _type), do: function_call("lower", [column_sql(arg)])
  defp function_sql("lcase", [arg], _type), do: function_call("lower", [column_sql(arg)])
  defp function_sql("upper", [arg], _type), do: function_call("upper", [column_sql(arg)])
  defp function_sql("ucase", [arg], _type), do: function_call("upper", [column_sql(arg)])
  defp function_sql("left", [arg1, arg2], _type), do: function_call("left", [column_sql(arg1), column_sql(arg2)])
  defp function_sql("right", [arg1, arg2], _type), do: function_call("right", [column_sql(arg1), column_sql(arg2)])
  defp function_sql("ltrim", [arg1], _type), do: function_call("ltrim", [column_sql(arg1)])
  defp function_sql("ltrim", [arg1, arg2], _type), do: function_call("ltrim", [column_sql(arg1), column_sql(arg2)])
  defp function_sql("rtrim", [arg1], _type), do: function_call("rtrim", [column_sql(arg1)])
  defp function_sql("rtrim", [arg1, arg2], _type), do: function_call("rtrim", [column_sql(arg1), column_sql(arg2)])
  defp function_sql("btrim", [arg1], _type), do: function_call("btrim", [column_sql(arg1)])
  defp function_sql("btrim", [arg1, arg2], _type), do: function_call("btrim", [column_sql(arg1), column_sql(arg2)])
  defp function_sql("substring", [arg1, arg2], _type), do:
    function_call("substring", [[column_sql(arg1), " FROM ", column_sql(arg2)]])
  defp function_sql("substring", [arg1, arg2, arg3], _type),
    do: function_call("substring", [[column_sql(arg1), " FROM ", column_sql(arg2), " FOR ", column_sql(arg3)]])
  defp function_sql("substring_for", [arg1, arg2], _type), do:
    function_call("substring", [[column_sql(arg1), "FOR ", column_sql(arg2)]])
  # misc functions
  defp function_sql("coalesce", args, _type), do: function_call("coalesce", [columns_sql(args)])
  defp function_sql({:cast, type}, [arg], _type), do: cast(column_sql(arg), sql_type(type))
  # binary operators
  for binary_operator <- ["+", "-", "*", "^"] do
    defp function_sql(unquote(binary_operator), [arg1, arg2], _type) do
      binary_operator_call(unquote(binary_operator), column_sql(arg1), column_sql(arg2))
    end
  end
  defp function_sql("/", [arg1, arg2], _type) do
    cast(binary_operator_call("/", column_sql(arg1), column_sql(arg2)), "float")
  end

  defp cast(expr, type) do
    function_call("cast", [[expr, " AS ", type]])
  end

  defp function_call(name, args) do
    [name, "(", Enum.intersperse(args, ",") ,")"]
  end

  defp binary_operator_call(operator, arg1, arg2) do
    ["(", arg1, operator, arg2, ")"]
  end

  defp from_clause({:join, join}, query) do
    ["(", from_clause(join.lhs, query), " ", join_sql(join.type), " ", from_clause(join.rhs, query),
      on_clause(join.conditions), ")"]
  end
  defp from_clause({:subquery, subquery}, _query) do
    ["(", build_fragments(subquery.ast), ") AS ", subquery.alias]
  end
  defp from_clause(table_name, query) when is_binary(table_name) do
    query.selected_tables
    |> Enum.find(&(&1.name == table_name))
    |> table_to_from()
  end

  defp on_clause([]), do: []
  defp on_clause([_|_] = conditions), do: [" ON ", conditions_to_fragments(conditions)]

  defp join_sql(:cross_join), do: "CROSS JOIN"
  defp join_sql(:inner_join), do: "INNER JOIN"
  defp join_sql(:full_outer_join), do: "FULL OUTER JOIN"
  defp join_sql(:left_outer_join), do: "LEFT OUTER JOIN"
  defp join_sql(:right_outer_join), do: "RIGHT OUTER JOIN"

  defp table_to_from(%{name: table_name, db_name: table_name}), do: table_name
  defp table_to_from(table), do: "#{table.db_name} AS \"#{table.name}\""

  defp sql_type(:integer), do: "integer"
  defp sql_type(:real), do: "float"
  defp sql_type(:boolean), do: "bool"
  defp sql_type(:timestamp), do: "timestamp"
  defp sql_type(:time), do: "time"
  defp sql_type(:date), do: "date"
  defp sql_type(:text), do: "text"

  defp where_fragments([]), do: []
  defp where_fragments(where_clause) do
    ["WHERE ", conditions_to_fragments(where_clause)]
  end

  defp conditions_to_fragments([_|_] = and_clauses) do
    ["(", and_clauses |> Enum.map(&conditions_to_fragments/1) |> join(" AND "), ")"]
  end
  defp conditions_to_fragments({:comparison, what, comparator, value}) do
    [to_fragment(what), to_fragment(comparator), to_fragment(value)]
  end
  defp conditions_to_fragments({:in, what, values}) do
    [to_fragment(what), " IN (", values |> Enum.map(&to_fragment/1) |> join(", "), ")"]
  end
  defp conditions_to_fragments({:not, {:is, what, match}}) do
    [to_fragment(what), " IS NOT ", to_fragment(match)]
  end
  Enum.each([
    {:like, " LIKE "},
    {:ilike, " ILIKE "},
    {:is, " IS "},
  ], fn({keyword, fragment}) ->
    defp conditions_to_fragments({unquote(keyword), what, match}) do
      [to_fragment(what), unquote(fragment), to_fragment(match)]
    end
  end)

  defp to_fragment(string) when is_binary(string), do: string
  defp to_fragment(atom) when is_atom(atom), do: to_string(atom) |> String.upcase()
  defp to_fragment(%NaiveDateTime{} = value), do: [?', to_string(value), ?']
  defp to_fragment(%Time{} = value), do: [?', to_string(value), ?']
  defp to_fragment(%Date{} = value), do: [?', to_string(value), ?']
  defp to_fragment(%Column{constant?: true, value: value}), do: constant_to_fragment(value)
  defp to_fragment(%Column{} = column), do: "\"#{column.table.name}\".\"#{column.name}\""

  defp escape_string(string), do: String.replace(string, "'", "''")

  defp constant_to_fragment(value) when is_binary(value), do: [?', escape_string(value), ?']
  defp constant_to_fragment(value) when is_number(value), do: to_string(value)
  defp constant_to_fragment(value) when is_boolean(value), do: to_string(value)

  defp join([], _joiner), do: []
  defp join([el], _joiner), do: [el]
  defp join([first | rest], joiner), do: [first, joiner, join(rest, joiner)]

  defp group_by_fragments(%Query{subquery?: true, group_by: [_|_] = group_by}) do
    [
      "GROUP BY ",
      group_by |> Enum.map(&column_sql/1) |> Enum.intersperse(",")
    ]
  end
  defp group_by_fragments(_query), do: []
end
