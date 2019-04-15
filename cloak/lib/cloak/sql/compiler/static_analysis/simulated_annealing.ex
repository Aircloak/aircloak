defmodule Cloak.Sql.Compiler.StaticAnalysis.SimulatedAnnealing do
  def minimize(optimization_problem) do
    annealing_schedule = fn temp ->
      if temp <= 0.000000001 do
        nil
      else
        temp * 0.99
      end
    end

    anneal(optimization_problem.function, annealing_schedule, optimization_problem.input_ranges)
  end

  def anneal(function, annealing_schedule, input_bounds) do
    state = input_bounds |> Enum.map(fn {min, max} -> :rand.uniform(max - min) + min end)
    initial_temp = input_bounds |> Enum.map(fn {min, max} -> max - min end) |> Enum.max()

    best =
      initial_temp
      |> Stream.iterate(annealing_schedule)
      |> Stream.take_while(& &1)
      |> Enum.reduce(state, fn temp, state ->
        perturbed = perturb(state, input_bounds)
        if accept?(function.(perturbed), function.(state), temp), do: perturbed, else: state
      end)

    {function.(best), best}
  end

  def perturb(state, input_bounds) do
    index = state |> length() |> :rand.uniform()
    {min, max} = input_bounds |> Enum.at(index - 1)

    update_in(state, [Access.at(index - 1)], fn x ->
      max_scale = ((max - min) / 2) |> :math.log2() |> round()
      min_scale = if x == 0, do: -32, else: (x |> abs() |> :math.log2() |> round()) - 32

      scale = :rand.uniform(max_scale - min_scale) + min_scale

      if :rand.uniform() > 0.5 do
        (x - :math.pow(2, scale)) |> max(min)
      else
        (x + :math.pow(2, scale)) |> min(max)
      end
    end)
  end

  def accept?(new, old, temp) do
    if new <= old do
      true
    else
      :rand.uniform() > 1 / :math.log2(2 + temp)
    end
  end
end
