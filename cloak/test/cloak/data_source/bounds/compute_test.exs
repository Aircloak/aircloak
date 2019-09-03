defmodule Cloak.DataSource.Bounds.Compute.Test do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import StreamData

  alias Cloak.DataSource.Bounds.Compute

  describe ".max" do
    property "it's money-aligned" do
      check all data <- input_data(), cutoff <- integer(1..20) do
        case Compute.max(data, cutoff) do
          :error -> :ok
          {:ok, result} -> assert money_aligned?(result)
        end
      end
    end

    test "for positive numbers", do: assert({:ok, 5} = Compute.max([6, 4, 3, 7], 3))

    test "for negative numbers", do: assert({:ok, -5} = Compute.max([-3, -6, -4, -7], 3))

    test "for mostly-positive small values", do: assert({:ok, 1} = Compute.max([0.1, 0.6, 0.2, -0.3, 0.7], 3))

    test "for mostly-negative small values", do: assert({:ok, 0} = Compute.max([-0.1, -0.6, 0.2, -0.3, -0.7], 3))
  end

  describe ".min" do
    property "it's money-aligned" do
      check all data <- input_data(), cutoff <- integer(1..20) do
        case Compute.min(data, cutoff) do
          :error -> :ok
          {:ok, result} -> assert money_aligned?(result)
        end
      end
    end

    test "for positive numbers", do: assert({:ok, 5} = Compute.min([6, 4, 3, 7], 3))

    test "for negative numbers", do: assert({:ok, -5} = Compute.min([-3, -6, -4, -7], 3))

    test "for mostly-positive small values", do: assert({:ok, 0} = Compute.min([0.1, 0.6, 0.2, -0.3, 0.7], 3))

    test "for mostly-negative small values", do: assert({:ok, -1} = Compute.min([-0.1, -0.6, 0.2, -0.3, -0.7], 3))
  end

  describe ".extend" do
    test "simple case" do
      assert {1, 200} = Compute.extend({10, 20})
    end

    property "if computed max > min, then extends the other way" do
      check all number1 <- integer(), number2 <- integer() do
        {input_min, input_max} = [number1, number2] |> Enum.min_max()
        assert Compute.extend({input_max, input_min}) == Compute.extend({input_min, input_max})
      end
    end

    property "produces a larger bound containing the original" do
      check all number1 <- integer(), number2 <- integer() do
        {input_min, input_max} = [number1, number2] |> Enum.min_max()
        {output_min, output_max} = Compute.extend({input_min, input_max})

        assert input_min == 0 || output_min == 0 || sign(input_min) == sign(output_min)
        assert input_max == 0 || output_max == 0 || sign(input_max) == sign(output_max)
        assert output_min <= input_min
        assert output_max >= input_max
        assert output_max - output_min >= input_max - input_min
      end
    end
  end

  defp sign(x), do: if(x < 0, do: -1, else: 1)

  defp input_data() do
    one_of([
      list_of(float()),
      list_of(integer()),
      data_with_outliers(float(), &float/1),
      data_with_outliers(integer(), &min_max_integer/1)
    ])
  end

  defp data_with_outliers(default_item_generator, item_generator) do
    bind(list_of(default_item_generator, min_length: 1), fn items ->
      min = Enum.max(items)
      max = if(min < 0, do: min / 10, else: min * 10)

      list_of(item_generator.(min: min, max: max), min_length: 1, max_length: div(length(items), 10) + 1)
      |> map(fn outliers -> items ++ outliers end)
    end)
  end

  defp min_max_integer(options) do
    min = Keyword.get(options, :min, 0)
    max = Keyword.get(options, :max, 100) |> round()
    integer(min..max)
  end

  defp money_aligned?(number), do: number in money_aligned_numbers()

  defp money_aligned_numbers() do
    [1, 2, 5, 0, -1, -2, -5]
    |> Stream.iterate(fn items -> Enum.map(items, &(&1 * 10)) end)
    |> Stream.flat_map(& &1)
    |> Stream.take(1000)
  end
end
