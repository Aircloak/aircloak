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

    property "there is a large number of entries gteq max" do
      check all data <- input_data(), cutoff <- integer(1..20) do
        case Compute.max(data, cutoff) do
          :error ->
            :ok

          {:ok, result} ->
            assert Enum.count(data, &(&1 >= result)) >= cutoff
        end
      end
    end
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

    property "there is a large number of entries lteq min" do
      check all data <- input_data(), cutoff <- integer(1..20) do
        case Compute.min(data, cutoff) do
          :error ->
            :ok

          {:ok, result} ->
            assert Enum.count(data, &(&1 <= result)) >= cutoff
        end
      end
    end
  end

  describe ".extend" do
    test "simple case" do
      assert {1, 200} = Compute.extend({10, 20})
    end

    property "produces a larger bound containing the original" do
      check all number1 <- integer(), number2 <- integer() do
        {input_min, input_max} = [number1, number2] |> Enum.min_max()
        {output_min, output_max} = Compute.extend({input_min, input_max})

        assert sign(input_min) == sign(output_min)
        assert sign(input_max) == sign(input_max)
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
