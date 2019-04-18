defmodule Cloak.Sql.Compiler.StaticAnalysis do
  alias Cloak.Sql.{Query, Expression}

  def to_function(expression, bound_source) do
    leaves =
      expression |> get_in([Query.Lenses.leaf_expressions() |> Lens.reject(&Expression.constant?/1)]) |> Enum.uniq()

    {
      update_in(expression, [Query.Lenses.all_expressions()], &do_to_function(&1, leaves)),
      Enum.map(leaves, bound_source)
    }
  end

  def safe?(method, function, input_bounds, {from, _to}) do
    {minimum, _} = method.minimize(function, input_bounds)
    minimum >= from
  end

  defp do_to_function(%Expression{function: "*", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) * b.(inputs) end

  defp do_to_function(%Expression{function: "/", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) / b.(inputs) end

  defp do_to_function(%Expression{function: "-", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) - b.(inputs) end

  defp do_to_function(%Expression{function: "+", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) + b.(inputs) end

  defp do_to_function(%Expression{constant?: true, value: value}, _leaves), do: fn _inputs -> value end

  defp do_to_function(leaf, leaves) do
    index = Enum.find_index(leaves, &(&1 == leaf))
    false = is_nil(index)
    fn inputs -> Enum.at(inputs, index) end
  end
end
