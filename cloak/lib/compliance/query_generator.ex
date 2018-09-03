defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table
  import __MODULE__.Generation

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  defmodule Scaffold do
    @type from :: {:table, Map.t()} | {:join, t, t} | {:subquery, t}

    @type t :: %__MODULE__{from: from, complexity: integer}

    defstruct [:from, :complexity]
  end

  @doc """
  Generates a randomized query into the provided tables of the given complexity. The query will have the more
  conditions, subqueries, etc. the higher the complexity.
  """
  @spec generate_ast([Table.t()], non_neg_integer) :: ast
  def generate_ast(tables, complexity) do
    tables
    |> generate_scaffold(complexity)
    |> generate_query_from_scaffold()
  end

  @doc "Generates the SQL query string from the given AST."
  @spec ast_to_sql(ast) :: String.t()
  def ast_to_sql(ast), do: __MODULE__.Format.ast_to_sql(ast)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp generate_query_from_scaffold(scaffold) do
    {query, _tables} = query(scaffold)
    query
  end

  defp query(scaffold) do
    {from, tables} = from(scaffold.from)
    {{:query, nil, [select(tables, scaffold.complexity), from]}, tables}
  end

  defp select(_tables, _complexity) do
    {:select, nil, [{:function, "count", [{:star, nil, []}]}]}
  end

  defp from(scaffold) do
    {element, tables} = from_element(scaffold)
    {{:from, nil, [element]}, tables}
  end

  defp from_element({:table, table}), do: {{:table, table.name, []}, [table]}

  defp from_element({:subquery, scaffold}), do: subquery(scaffold)

  defp from_element({:join, scaffold1, scaffold2}) do
    {left, left_tables} = join_element(scaffold1)
    {right, right_tables} = join_element(scaffold2)

    {
      {:join, nil, [left, right, {:on, nil, [{:=, nil, [{:boolean, true, []}, {:boolean, false, []}]}]}]},
      left_tables ++ right_tables
    }
  end

  defp join_element(%{from: table = {:table, _}}), do: from_element(table)
  defp join_element(scaffold), do: subquery(scaffold)

  defp subquery(scaffold) do
    {query, tables} = query(scaffold)
    {{:as, name(scaffold.complexity), [{:subquery, nil, [query]}]}, tables}
  end

  defp name(complexity) do
    StreamData.string(?a..?z, min_length: 1) |> StreamData.resize(complexity) |> Enum.at(0)
  end

  defp generate_scaffold(tables, complexity) do
    frequency(complexity, %{
      3 => %Scaffold{from: {:table, Enum.random(tables)}, complexity: complexity},
      1 => %Scaffold{from: {:subquery, generate_scaffold(tables, div(complexity, 2))}, complexity: div(complexity, 2)},
      1 => %Scaffold{
        from: {:join, generate_scaffold(tables, div(complexity, 3)), generate_scaffold(tables, div(complexity, 3))},
        complexity: div(complexity, 3)
      }
    })
  end
end
