defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table
  import __MODULE__.Generation

  use Lens.Macros

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  defmodule Scaffold do
    @type from :: {:table, Map.t()} | {:join, t, t} | {:subquery, t}

    @type t :: %__MODULE__{from: from, complexity: integer, select_user_id?: boolean}

    defstruct [:from, :complexity, :select_user_id?]
  end

  @doc """
  Generates a randomized query into the provided tables of the given complexity. The query will have the more
  conditions, subqueries, etc. the higher the complexity.
  """
  @spec generate_ast([Table.t()], non_neg_integer) :: ast
  def generate_ast(tables, complexity) do
    tables
    |> generate_scaffold(complexity)
    |> set_select_user_id()
    |> generate_query_from_scaffold()
  end

  @doc "Generates the SQL query string from the given AST."
  @spec ast_to_sql(ast) :: String.t()
  def ast_to_sql(ast), do: __MODULE__.Format.ast_to_sql(ast)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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

  defp set_select_user_id(scaffold) do
    scaffold
    |> put_in([Lens.key(:select_user_id?)], false)
    |> update_in([sub_scaffolds()], fn
      scaffold = %{from: {:table, _}} ->
        %{scaffold | select_user_id?: boolean()}

      scaffold = %{from: {:subquery, _}} ->
        if boolean() do
          force_select_user_id(scaffold)
        else
          %{scaffold | select_user_id?: false}
        end

      scaffold = %{from: {:join, left, right}} ->
        if left.select_user_id? || right.select_user_id? do
          force_select_user_id(scaffold)
        else
          scaffold
        end
    end)
  end

  defp force_select_user_id(scaffold) do
    put_in(scaffold, [Lens.both(sub_scaffolds(), Lens.root()) |> Lens.key(:select_user_id?)], true)
  end

  deflensp sub_scaffolds() do
    Lens.key(:from)
    |> Lens.match(fn
      {:table, _} -> Lens.empty()
      {:subquery, _} -> Lens.index(1)
      {:join, _, _} -> Lens.indices([1, 2])
    end)
    |> Lens.recur()
  end

  defp generate_query_from_scaffold(scaffold) do
    {query, _tables} = query(scaffold)
    query
  end

  defp query(scaffold) do
    {from, tables} = from(scaffold.from)
    {{:query, nil, [select(scaffold, tables), from]}, tables}
  end

  defp select(scaffold, [table | _]) do
    if scaffold.select_user_id? do
      {:select, nil, [{:column, nil, [{:unquoted, table.user_id, []}]}]}
    else
      {:select, nil, [{:function, "count", [{:star, nil, []}]}]}
    end
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

  defp boolean(), do: Enum.random([true, false])

  defp name(complexity) do
    StreamData.string(?a..?z, min_length: 1) |> StreamData.resize(complexity) |> Enum.at(0)
  end
end
