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
    {minimum, _} = minimize(method, function, input_bounds)
    minimum >= from
  catch
    :crash -> false
  end

  defp minimize(method, function, input_bounds) do
    function = fn inputs ->
      try do
        function.(inputs)
      rescue
        _ -> throw(:crash)
      end
    end

    method.minimize(function, input_bounds)
  end

  defp do_to_function(%Expression{function: "*", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) * b.(inputs) end

  defp do_to_function(%Expression{function: "/", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) / b.(inputs) end

  defp do_to_function(%Expression{function: "-", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) - b.(inputs) end

  defp do_to_function(%Expression{function: "+", function_args: [a, b]}, _leaves),
    do: fn inputs -> a.(inputs) + b.(inputs) end

  defp do_to_function(%Expression{function: "abs", function_args: [a]}, _leaves),
    do: fn inputs -> abs(a.(inputs)) end

  defp do_to_function(%Expression{constant?: true, value: value}, _leaves), do: fn _inputs -> value end

  defp do_to_function(leaf, leaves) do
    index = Enum.find_index(leaves, &(&1 == leaf))
    false = is_nil(index)
    fn inputs -> Enum.at(inputs, index) end
  end
end
