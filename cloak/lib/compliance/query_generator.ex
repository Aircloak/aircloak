defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table
  import __MODULE__.Generation

  use Lens.Macros

  defmodule Scaffold do
    @type from :: {:table, Map.t()} | {:join, t, t} | {:subquery, t}

    @type t :: %__MODULE__{from: from, complexity: integer, select_user_id?: boolean}

    defstruct [:from, :complexity, :select_user_id?]
  end

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generates a randomized query with `generate_ast/2`. Also returns the random seed used to generate the query."
  @spec ast_with_seed([Table.t()], non_neg_integer) :: {ast, String.t()}
  def ast_with_seed(tables, complexity) do
    if :rand.export_seed() == :undefined do
      :rand.uniform()
    end

    seed = "#{:rand.export_seed() |> :erlang.term_to_binary() |> Base.encode64()}:#{complexity}"
    {generate_ast(tables, complexity), seed}
  end

  @doc "Generates a query from a seed produced by `ast_with_seed`."
  @spec ast_from_seed(String.t(), [Table.t()]) :: ast
  def ast_from_seed(seed, tables) do
    [seed, complexity] = String.split(seed, ":")
    complexity = String.to_integer(complexity)
    seed = seed |> Base.decode64!() |> :erlang.binary_to_term()

    :rand.seed(seed)
    generate_ast(tables, complexity)
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
    |> update_in([all_scaffolds()], fn
      scaffold = %{from: {:table, _}} ->
        %{scaffold | select_user_id?: true}

      scaffold = %{from: {:subquery, _}} ->
        if boolean() do
          force_select_user_id(scaffold)
        else
          %{scaffold | select_user_id?: false}
        end

      scaffold = %{from: {:join, left, right}} ->
        if left.select_user_id? or right.select_user_id? do
          force_select_user_id(scaffold)
        else
          %{scaffold | select_user_id?: false}
        end
    end)
    |> put_in([Lens.key(:select_user_id?)], false)
  end

  defp force_select_user_id(scaffold) do
    put_in(scaffold, [all_scaffolds() |> Lens.key(:select_user_id?)], true)
  end

  deflensp(all_scaffolds(), do: Lens.both(sub_scaffolds(), Lens.root()))

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
    {from, tables} = from(scaffold)
    {{:query, nil, [select(scaffold, tables), from]}, tables}
  end

  defp select(scaffold, tables) do
    if scaffold.select_user_id? do
      {:select, nil, [user_id_from_tables(tables)]}
    else
      {:select, nil, [{:function, "count", [{:star, nil, []}]}]}
    end
  end

  defp from(scaffold) do
    {element, tables} = from_element(scaffold)
    {{:from, nil, [element]}, tables}
  end

  defp from_element(%{from: {:table, table}}), do: {{:table, table.name, []}, [table]}

  defp from_element(%{from: {:subquery, scaffold}}), do: subquery(scaffold)

  defp from_element(%{from: {:join, left_scaffold, right_scaffold}}) do
    {left, left_tables} = join_element(left_scaffold)
    {right, right_tables} = join_element(right_scaffold)

    {
      {:join, nil, [left, right, on(left_scaffold.select_user_id?, left_tables, right_tables)]},
      left_tables ++ right_tables
    }
  end

  defp on(join_on_user_id?, left_tables, right_tables),
    do: {:on, nil, on_conditions(join_on_user_id?, left_tables, right_tables)}

  defp on_conditions(join_on_user_id?, left_tables, right_tables) do
    if join_on_user_id? do
      [{:=, nil, [user_id_from_tables(left_tables), user_id_from_tables(right_tables)]}]
    else
      [{:=, nil, [{:boolean, true, []}, {:boolean, false, []}]}]
    end
  end

  defp user_id_from_tables([table | _]), do: {:column, nil, [{:unquoted, table.user_id, []}]}

  defp join_element(scaffold = %{from: {:table, _}}), do: from_element(scaffold)
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
