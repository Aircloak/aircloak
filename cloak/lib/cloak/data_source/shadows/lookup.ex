defmodule Cloak.DataSource.Shadows.Lookup do
  alias Cloak.Sql.{Query, Expression}

  def any?(expression, value, shadow) do
    Enum.any?(shadow, fn candidate ->
      evaluate(expression, candidate) == value
    end)
  end

  defp evaluate(expression, candidate) do
    expression
    |> put_in(
      [Query.Lenses.leaf_expressions() |> Lens.filter(&Expression.column?/1)],
      Expression.constant(:ignored, candidate)
    )
    |> Expression.const_value()
  end
end
