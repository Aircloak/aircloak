defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table

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

  defmacrop generate(complexity, {:%{}, line, options}) do
    options =
      Enum.map(options, fn {key, val} ->
        {key,
         quote do
           fn -> unquote(val) end
         end}
      end)

    quote do
      do_generate(unquote(complexity), unquote(options))
    end
  end

  defp generate_query_from_scaffold(%{from: {:subquery, scaffold}}) do
    {:query, nil, [select_ast(), {:from, nil, [{:subquery, nil, [generate_query_from_scaffold(scaffold)]}]}]}
  end

  defp generate_query_from_scaffold(%{from: {:join, scaffold1, scaffold2}}) do
    {:query, nil,
     [
       select_ast(),
       {:from, nil,
        [
          {:join, nil,
           [
             generate_query_from_scaffold(scaffold1),
             generate_query_from_scaffold(scaffold2),
             {:on, nil, [{:=, nil, [{:boolean, true, []}, {:boolean, false, []}]}]}
           ]}
        ]}
     ]}
  end

  defp generate_query_from_scaffold(%{from: table}) do
    {:query, nil, [select_ast(), from_table(table)]}
  end

  defp select_ast() do
    {:select, nil, [{:function, "count", [{:star, nil, []}]}]}
  end

  defp from_table(table) do
    {:from, nil, [{:table, table.name, []}]}
  end

  defp generate_scaffold(tables, complexity) do
    generate(complexity, %{
      3 => %Scaffold{from: Enum.random(tables), complexity: complexity},
      1 => %Scaffold{from: {:subquery, generate_scaffold(tables, div(complexity, 2))}, complexity: div(complexity, 2)},
      1 => %Scaffold{
        from: {:join, generate_scaffold(tables, div(complexity, 3)), generate_scaffold(tables, div(complexity, 3))},
        complexity: div(complexity, 3)
      }
    })
  end

  defp do_generate(complexity, options) do
    sum = options |> Enum.map(&elem(&1, 0)) |> Enum.sum()
    random = :rand.uniform(sum |> min(complexity) |> max(1)) - 1
    pick_option(options, random)
  end

  defp pick_option([{frequency, option} | _], number) when number < frequency, do: option.()
  defp pick_option([{frequency, _} | options], number), do: pick_option(options, number - frequency)
end
