defmodule Cloak.Sql.Compiler.StaticAnalysis.Test do
  alias Cloak.Sql.Compiler.StaticAnalysis

  use ExUnit.Case

  test "run" do
    for method <- methods() do
      assert StaticAnalysis.within_range?(optimization_problem(), method, {-100_000, 100_000})
      refute StaticAnalysis.within_range?(optimization_problem(), method, {0, 10000})
    end
  end

  def methods() do
    [StaticAnalysis.SimulatedAnnealing]
  end

  def optimization_problem() do
    %{
      function: fn [x, y, z] -> x + y + z end,
      input_ranges: [
        {10, 1000},
        {0, 200},
        {-100, -10}
      ]
    }
  end
end
