defmodule Cloak.Sql.Compiler.ASTNormalization do
  alias Cloak.Sql.Function

  def normalize(ast), do: rewrite_distinct(ast)

  defp rewrite_distinct(ast = %{distinct?: false}), do: ast
  defp rewrite_distinct(ast = %{distinct?: true, columns: columns, from: from, group_by: group_by = [_ | _]}), do:
    Map.merge(ast, %{
      distinct?: false,
      columns: [:*],
      from: {:subquery, %{
        alias: "to_fix",
        ast: %{
          command: :select,
          distinct?: false,
          columns: columns,
          from: from,
          group_by: group_by,
        },
      }},
      group_by: Enum.map(1..length(columns), &{:constant, :integer, &1}),
    })
  defp rewrite_distinct(ast = %{distinct?: true, columns: columns}), do:
    if Enum.any?(columns, &aggregator?/1),
      do: %{ast | distinct?: false},
      else: Map.merge(ast, %{distinct?: false, group_by: columns})

  defp aggregator?({:function, name, args}), do:
    Function.has_attribute?(name, :aggregator) or Enum.any?(args, &aggregator?/1)
  defp aggregator?(_), do: false
end
