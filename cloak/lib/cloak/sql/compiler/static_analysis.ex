defmodule Cloak.Sql.Compiler.StaticAnalysis do
  def within_range?(optimization_problem, method, {from, to}) do
    {minimum, _} = method.minimize(optimization_problem)
    minimum >= from
  end
end
