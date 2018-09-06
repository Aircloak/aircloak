defmodule Cloak.Compliance.QueryGenerator do
  @moduledoc "Provides utilities for randomly generating queries into an arbitrary set of tables."

  @type ast :: {atom, any, [ast]}

  alias Cloak.DataSource.Table
  alias Cloak.Sql.Function
  import __MODULE__.Generation

  use Lens.Macros

  defmodule Scaffold do
    @moduledoc "Represents the high-level structure of a query to be generated."

    @type from :: {:aliased_table, Map.t()} | {:table, Map.t()} | {:join, t, t} | {:subquery, t}

    @type t :: %__MODULE__{from: from, complexity: integer, select_user_id?: boolean, aggregate?: boolean}

    defstruct [:from, :complexity, :select_user_id?, :aggregate?]
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
    |> resolve_table_name_clashes()
    |> generate_query_from_scaffold()
  end

  @doc "Generates the SQL query string from the given AST."
  @spec ast_to_sql(ast) :: String.t()
  def ast_to_sql(ast), do: __MODULE__.Format.ast_to_sql(ast)

  # -------------------------------------------------------------------
  # Scaffold generation
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
    |> put_in([Lens.key(:aggregate?)], boolean())
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

  defp resolve_table_name_clashes(scaffold) do
    update_in(scaffold, [all_scaffolds()], fn
      scaffold = %{from: {:join, left = %{from: {:table, %{name: name}}}, right = %{from: {:table, %{name: name}}}}} ->
        %{scaffold | from: {:join, left, set_table_alias(right)}}

      other ->
        other
    end)
  end

  defp set_table_alias(scaffold) do
    update_in(scaffold, [Lens.key(:from)], fn {:table, table} -> {:aliased_table, table} end)
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

  # -------------------------------------------------------------------
  # AST generation
  # -------------------------------------------------------------------

  defp generate_query_from_scaffold(scaffold) do
    {query, _tables} = query(scaffold)
    query
  end

  defp query(scaffold) do
    {from, tables} = from(scaffold)
    {select, selected_tables} = select(scaffold, tables)
    order_by = order_by(scaffold, select)
    limit = limit(scaffold, order_by)

    {
      {:query, nil,
       [
         select,
         from,
         where(scaffold),
         group_by(scaffold, select),
         having(scaffold),
         order_by,
         limit,
         offset(scaffold, order_by, limit),
         sample_users(scaffold)
       ]},
      selected_tables
    }
  end

  defp select(scaffold, tables) do
    {elements, tables} =
      if scaffold.select_user_id? do
        select_elements_with_user_id(scaffold, tables)
      else
        select_elements(scaffold, tables)
      end

    {{:select, nil, elements}, tables}
  end

  defp select_elements_with_user_id(scaffold, tables) do
    {elements, [table]} = select_elements(scaffold, tables)
    {column, name, type} = user_id_from_tables(tables)

    {[column | elements], [%{user_id: name, columns: [%{name: name, type: type} | table.columns]}]}
  end

  defp select_elements(scaffold, _tables) do
    if scaffold.aggregate? do
      {
        [{:function, "count", [{:star, nil, []}]}],
        [%{user_id: nil, columns: [%{name: "count", type: :integer}]}]
      }
    else
      name = name(scaffold.complexity)

      {
        [{:as, name, [{:text, "hello", []}]}],
        [%{user_id: nil, columns: [%{name: name, type: :text}]}]
      }
    end
  end

  defp from(scaffold) do
    {element, tables} = from_element(scaffold)
    {{:from, nil, [element]}, tables}
  end

  defp from_element(%{from: {:table, table}}), do: {{:table, table.name, []}, [table]}

  defp from_element(%{complexity: complexity, from: {:aliased_table, table}}) do
    name = name(complexity)
    {{:as, name, [{:table, table.name, []}]}, [%{table | name: name}]}
  end

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
      {left_id, _, _} = user_id_from_tables(left_tables)
      {right_id, _, _} = user_id_from_tables(right_tables)
      [{:=, nil, [left_id, right_id]}]
    else
      [{:=, nil, [{:boolean, true, []}, {:boolean, false, []}]}]
    end
  end

  defp user_id_from_tables([table | _]) do
    name = table.user_id
    %{type: type} = Enum.find(table.columns, &match?(%{name: ^name}, &1))
    {{:column, nil, [{:unquoted, table.name, []}, {:unquoted, table.user_id, []}]}, name, type}
  end

  defp join_element(scaffold = %{from: {:table, _}}), do: from_element(scaffold)
  defp join_element(scaffold = %{from: {:aliased_table, _}}), do: from_element(scaffold)
  defp join_element(scaffold), do: subquery(scaffold)

  defp subquery(scaffold) do
    {query, [table]} = query(scaffold)
    name = name(scaffold.complexity)
    {{:as, name, [{:subquery, nil, [query]}]}, [Map.put(table, :name, name)]}
  end

  defp where(scaffold) do
    frequency(scaffold.complexity, %{
      1 => {:where, nil, [where_condition(scaffold.complexity)]},
      1 => empty()
    })
  end

  defp where_condition(complexity) do
    frequency(complexity, %{
      2 => simple_condtion(),
      1 => {:and, nil, [where_condition(div(complexity, 2)), where_condition(div(complexity, 2))]}
    })
  end

  defp simple_condtion(), do: {:=, nil, [constant(), constant()]}

  defp constant(), do: {:boolean, boolean(), []}

  defp group_by(%{aggregate?: false}, _select), do: empty()

  defp group_by(_scaffold, select) do
    case group_by_elements(select) do
      [] -> empty()
      elements -> {:group_by, nil, elements}
    end
  end

  defp group_by_elements({:select, _, items}) do
    items
    |> Enum.with_index(1)
    |> Enum.reject(fn {expression, _} -> aggregate_expression?(expression) end)
    |> Enum.map(fn {_, index} -> {:integer, index, []} end)
  end

  defp aggregate_expression?(expression),
    do: expression |> get_in([all_expressions()]) |> Enum.any?(&aggregate_function?/1)

  deflensp all_expressions() do
    Lens.both(Lens.index(2) |> Lens.all() |> Lens.recur(), Lens.root())
  end

  defp aggregate_function?({:function, name, _}), do: Function.aggregator?(name)
  defp aggregate_function?(_), do: false

  defp having(%{aggregate?: false}), do: empty()

  defp having(scaffold), do: {:having, nil, [where_condition(scaffold.complexity)]}

  defp order_by(scaffold, select) do
    case order_by_elements(scaffold, select) do
      [] -> empty()
      elements -> {:order_by, nil, elements}
    end
  end

  defp order_by_elements(scaffold, {:select, _, items}) do
    frequency(scaffold.complexity, %{
      1 => [],
      1 => [{:integer, :rand.uniform(length(items)), []}]
    })
  end

  defp sample_users(%{select_user_id?: false}), do: empty()

  defp sample_users(scaffold) do
    frequency(scaffold.complexity, %{
      1 => empty(),
      1 => {:sample_users, sample_users_size(scaffold.complexity), []}
    })
  end

  defp limit(_scaffold, _order_by = {:empty, _, _}), do: empty()

  defp limit(scaffold, _order_by) do
    frequency(scaffold.complexity, %{
      1 => empty(),
      1 => {:limit, :rand.uniform(100), []}
    })
  end

  defp offset(%{select_user_id?: true}, _order_by, _limit = {:empty, _, _}), do: empty()

  defp offset(_scaffold, _order_by = {:empty, _, _}, _limit), do: empty()

  defp offset(scaffold, _order_by, _limit) do
    frequency(scaffold.complexity, %{
      1 => empty(),
      1 => {:offset, :rand.uniform(100), []}
    })
  end

  defp sample_users_size(complexity) do
    frequency(complexity, %{
      1 => :rand.uniform() * 100,
      1 => :rand.uniform() * 10,
      1 => :rand.uniform()
    })
  end

  # -------------------------------------------------------------------
  # Simple generators
  # -------------------------------------------------------------------

  defp boolean(), do: Enum.random([true, false])

  defp name(complexity) do
    StreamData.string(?a..?z, min_length: 1) |> StreamData.resize(complexity) |> Enum.at(0)
  end

  defp empty(), do: {:empty, nil, []}
end
