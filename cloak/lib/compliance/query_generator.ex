defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table
  import __MODULE__.Generation

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  defmodule Scaffold do
    @type from :: {:table, Map.t()} | {:join, from, from} | {:subquery, from}

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

  defp generate_query_from_scaffold(%{from: from, complexity: complexity}) do
    {:query, nil, [select_ast(complexity), from(from)]}
  end

  defp select_ast(_complexity) do
    {:select, nil, [{:function, "count", [{:star, nil, []}]}]}
  end

  defp from({:subquery, scaffold}) do
    {:from, nil, [{:subquery, nil, [generate_query_from_scaffold(scaffold)]}]}
  end

  defp from({:join, scaffold1, scaffold2}) do
    {:from, nil,
     [
       {:join, nil,
        [
          generate_query_from_scaffold(scaffold1),
          generate_query_from_scaffold(scaffold2),
          {:on, nil, [{:=, nil, [{:boolean, true, []}, {:boolean, false, []}]}]}
        ]}
     ]}
  end

  defp from(table) do
    {:from, nil, [{:table, table.name, []}]}
  end

  defp generate_scaffold(tables, complexity) do
    frequency(complexity, %{
      3 => %Scaffold{from: Enum.random(tables), complexity: complexity},
      1 => %Scaffold{from: {:subquery, generate_scaffold(tables, div(complexity, 2))}, complexity: div(complexity, 2)},
      1 => %Scaffold{
        from: {:join, generate_scaffold(tables, div(complexity, 3)), generate_scaffold(tables, div(complexity, 3))},
        complexity: div(complexity, 3)
      }
    })
  end
end
