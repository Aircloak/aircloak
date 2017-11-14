defmodule Cloak.Sql.Compiler.ASTNormalization do
  @moduledoc "Deals with normalizing the query AST so that less cases must be handled downstream."

  alias Cloak.Sql.{Function, Parser, Compiler.Helpers}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Rewrites the query AST, producing one with the same semantics that is simpler to process.

  Currently it only replaces DISTINCT usage in SELECT lists with an equivalent GROUP BY (possibly adding a subquery).
  """
  @spec normalize(Parser.parsed_query) :: Parser.parsed_query
  def normalize(ast), do: Helpers.apply_bottom_up(ast, &rewrite_distinct/1)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp rewrite_distinct(ast = %{distinct?: true, columns: columns, from: from, group_by: group_by = [_ | _]}), do:
    Map.merge(ast, %{
      distinct?: false,
      columns: [:*],
      from: {:subquery, %{
        alias: unique_alias(),
        ast: %{
          command: :select,
          distinct?: false,
          columns: columns,
          from: from,
          group_by: group_by,
        },
      }},
      group_by: grouping_clause(columns),
    })
  defp rewrite_distinct(ast = %{distinct?: true, columns: columns}), do:
    if Enum.any?(columns, &aggregator?/1),
      do: %{ast | distinct?: false},
      else: Map.merge(ast, %{distinct?: false, group_by: grouping_clause(columns)})
  defp rewrite_distinct(ast), do: ast

  defp grouping_clause(columns), do:
    Enum.map(1..length(columns), &{:constant, :integer, &1})

  defp aggregator?({:function, name, args}), do:
    Function.has_attribute?(name, :aggregator) or Enum.any?(args, &aggregator?/1)
  defp aggregator?(_), do: false

  # We set a unique alias on generated subqueries so that they don't clash with user aliases or tables
  defp unique_alias(), do: "__ac_alias__#{System.unique_integer([:positive])}"
end
