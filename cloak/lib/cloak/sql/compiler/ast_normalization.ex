defmodule Cloak.Sql.Compiler.ASTNormalization do
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
    Map.merge(ast, %{distinct?: false, group_by: columns})
end
