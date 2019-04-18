defmodule Cloak.Sql.Compiler.StaticAnalysis do
  def to_function(expression, bound_source) do
    {
      fn [x] -> x * x end,
      [{100, 200}]
    }
  end

  def safe?(method, function, input_bounds, {from, _to}) do
    {minimum, _} = method.minimize(function, input_bounds)
    minimum >= from
  end
end
